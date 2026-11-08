"""
HMB modeli - NIHAI HESAP (analitik + sabit-tohumlu PSA)
=========================================================
Iki bolum:
  A) NOKTA TAHMINLER: kapali-form, rastgelelik YOK -> her calistirmada AYNI.
     Makaledeki Tablo 5,6(kat),7-esik ve tasarruf% degerleri buradan gelir.
  B) OLASILIKLAR (P<IV, P<oral, en-zorlu-test): PSA, tek sabit tohumla,
     PYTHONHASHSEED'den bagimsiz. Belirsizlik aralik (GA) icin.
Makaledeki her rakamin kaynagi burasidir. Parametre degistir -> calistir -> tablolari guncelle.
"""
import numpy as np

# ================= PARAMETRELER =================
MLP, CYC, IV, USD = 0.80, 13.0, 8.25, 47.2  # IV: 2x FCM flakon + 1 seans / 1000mg
FE_TARGET = 0.50          # hedef-durum (Tablo 3, deterministik)
FE_POP    = 0.43          # populasyon ort. (0.36-0.50 ort.) - olasilik/PSA icin
BOX, ELEMBOX = 152.62, 2400.0  # Tardyferon 30 tb x 80 mg
PBACS = [200,400,800,1000]
# prevensiyon: MBL azalma, yillik TL
PREV = {"NSAID":(0.35,709), "TXA":(0.45,2345), "OKS-E2V/DNG":(0.71,7344), "OKS-LNG/EE":(0.45,4556), "LNG-IUS":(0.82,862)}
ORD  = ["LNG-IUS","NSAID","TXA","OKS-LNG/EE","OKS-E2V/DNG"]
# oral: emilim, IV-gecis
ORAL = {"gunluk":(0.163,0.30), "gun-asiri":(0.218,0.25)}

def tr(x,d=2): return f"{x:.{d}f}".replace('.',',')

# ============ A) NOKTA TAHMINLER (analitik) ============
def iy(p, fe): return fe*MLP*p*CYC

# oral yol harmanli maliyet (kaybedilen mg basina) - deterministik
oral_cost = {k:(1-pf)*(BOX/(ELEMBOX*f))+pf*IV for k,(f,pf) in ORAL.items()}
cheapest  = min(min(oral_cost.values()), IV)

print("="*76)
print("A) NOKTA TAHMINLER (kapali-form, rastgelelik yok - her zaman ayni)")
print("="*76)
print(f"\nReplasman yollari (kaybedilen mg basina, fe kullanilmaz):")
for k,v in oral_cost.items(): print(f"  oral {k:<10} {tr(v)} TL/mg (${v/USD:.3f})")
print(f"  IV            {tr(IV)} TL/mg (${IV/USD:.3f})")
print(f"  en ucuz replasman = {tr(cheapest)} TL/mg")

print(f"\nTablo 3 - demir kaybi (fe={FE_TARGET}, hedef durum):")
for p in PBACS:
    yr=FE_TARGET*MLP*p*CYC
    print(f"  PBAC {p:>4}: {MLP*p:.0f} mL, {FE_TARGET*MLP*p:.0f} mg/siklus, {yr:.0f} mg/yil, IV {yr*IV:.0f} TL (${yr*IV/USD:.0f})")

print(f"\nTablo 5 - onlenen mg/mg (fe={FE_POP}, populasyon) + Tablo 6 kat + tasarruf%:")
print(f"  {'Yontem':<9}"+"".join(f"PBAC{p:<6}" for p in PBACS))
T5={}
for m in ORD:
    mbl,tl=PREV[m]; row=[]
    for p in PBACS:
        pm=tl/(mbl*iy(p,FE_POP)); row.append(pm)
    T5[m]=row
    print(f"  {m:<9}"+"".join(f"{tr(v):<10}" for v in row))
print(f"\n  vsIV kat (IV/pm):")
for m in ORD:
    print(f"  {m:<9}"+"".join(f"{IV/v:<10.0f}" for v in T5[m]))
print(f"\n  tasarruf% (r - c/(iy*IV)):")
for m in ORD:
    mbl,tl=PREV[m]
    print(f"  {m:<9}"+"".join(f"{(mbl - tl/(iy(p,FE_POP)*IV))*100:<10.1f}" for p in PBACS))

print(f"\nTablo 7-esik: prevensiyon = en ucuz replasman ({tr(cheapest)}) oldugu kritik nokta")
print(f"  Her yontem icin: pm < {tr(cheapest)} mi? (fe={FE_POP} nokta tahmin)")
for m in ORD:
    print(f"  {m:<9}"+"".join(f"{'EVET' if v<cheapest else 'hayir':<10}" for v in T5[m]))

# ============ B) OLASILIKLAR (PSA, tek sabit tohum) ============
print("\n"+"="*76)
print("B) OLASILIKLAR (PSA, sabit tohum=12345, PYTHONHASHSEED-bagimsiz)")
print("="*76)
np.random.seed(12345)                      # eski API: global, deterministik
N=200000                                   # buyuk N -> Monte Carlo gurultusu ihmal edilir
def beta_ms(m,s): 
    v=s**2;k=m*(1-m)/v-1;return np.random.beta(m*k,(1-m)*k,N)
def gamma_ms(m,s): return np.random.gamma((m/s)**2,s**2/m,N)
# SIRA SABIT (asla degistirme):
fe_s=np.random.uniform(0.36,0.50,N)
mlp_s=np.clip(np.random.normal(0.80,0.12,N),0.45,1.15)
iv_s=np.full(N, 8.25)  # IV sabit (islem bedeli doza-bagli ama ~8.25)
box_s=gamma_ms(152.62,23)
pr={}
for m in ORD:
    mbl,tl=PREV[m]
    ss={"NSAID":0.05,"TXA":0.05,"OKS-E2V/DNG":0.04,"OKS-LNG/EE":0.05,"LNG-IUS":0.05}[m]
    cs={"NSAID":180,"TXA":590,"OKS-E2V/DNG":1830,"OKS-LNG/EE":1140,"LNG-IUS":215}[m]
    pr[m]=(beta_ms(mbl,ss),gamma_ms(tl,cs))
fd=beta_ms(0.163,0.030);pfd=beta_ms(0.30,0.07);fa=beta_ms(0.218,0.035);pfa=beta_ms(0.25,0.07)
od=(1-pfd)*(box_s/(ELEMBOX*fd))+pfd*iv_s; oa=(1-pfa)*(box_s/(ELEMBOX*fa))+pfa*iv_s
ch=np.minimum(np.minimum(od,oa),iv_s)
def iys(p): return fe_s*mlp_s*p*CYC

print("\nTablo 6 - P(<IV):")
for m in ORD:
    r,c=pr[m]; print(f"  {m:<9}"+"".join(f"%{np.mean((c/(r*iys(p)))<iv_s)*100:<9.0f}" for p in PBACS))
print("\nTablo 7 - P(<en ucuz oral):")
for m in ORD:
    r,c=pr[m]; print(f"  {m:<9}"+"".join(f"%{np.mean((c/(r*iys(p)))<ch)*100:<9.0f}" for p in PBACS))
print("\nEn zorlu test:")
for p in PBACS:
    a=np.vstack([c/(r*iys(p)) for r,c in pr.values()])
    print(f"  PBAC {p}: %{np.mean(a.max(0)<ch)*100:.1f}")
print("\nGA ornekleri (belirsizlik):")
r,c=pr["LNG-IUS"]
for p in [200,800]:
    pm=c/(r*iys(p)); print(f"  LNG PBAC{p} onlenen mg/mg: %95 GA {np.percentile(pm,2.5):.2f}-{np.percentile(pm,97.5):.2f}")

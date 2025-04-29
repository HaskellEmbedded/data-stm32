module {{ modns }}
  ( eth
  ) where

import Ivory.Language
import Ivory.HW

import Ivory.BSP.STM32{{ dev }}.AF
import Ivory.BSP.STM32{{ dev }}.RCC
import Ivory.BSP.STM32{{ dev }}.MemoryMap
import Ivory.BSP.STM32{{ dev }}.SYSCFG
import qualified Ivory.BSP.STM32{{ dev }}.Interrupt as {{ dev }}

import Ivory.BSP.STM32.AF
import Ivory.BSP.STM32.Peripheral.ETH

ethdma :: ETHDMA
ethdma = mkETHDMA ethernet_dma_periph_base

mac :: MAC
mac =
  mkMAC
    ethernet_mac_periph_base
    rccenable
    rccdisable
    rccrxenable
    rccrxdisable
    rcctxenable
    rcctxdisable
    {{ dev }}.ETH
  where
  rccenable    = modifyReg rcc_reg_ahb1enr $ setBit   rcc_ahb1enr_ethmacen
  rccdisable   = modifyReg rcc_reg_ahb1enr $ clearBit rcc_ahb1enr_ethmacen
  rccrxenable  = modifyReg rcc_reg_ahb1enr $ setBit   rcc_ahb1enr_ethmacrxen
  rccrxdisable = modifyReg rcc_reg_ahb1enr $ clearBit rcc_ahb1enr_ethmacrxen
  rcctxenable  = modifyReg rcc_reg_ahb1enr $ setBit   rcc_ahb1enr_ethmactxen
  rcctxdisable = modifyReg rcc_reg_ahb1enr $ clearBit rcc_ahb1enr_ethmactxen

mmc :: MMC
mmc = mkMMC ethernet_mmc_periph_base

ptp :: PTP
ptp =
  mkPTP
    ethernet_mmc_periph_base
    rccenable
    rccdisable
  where
  rccenable    = modifyReg rcc_reg_ahb1enr $ setBit   rcc_ahb1enr_ethmacptpen
  rccdisable   = modifyReg rcc_reg_ahb1enr $ clearBit rcc_ahb1enr_ethmacptpen

eth :: ETH
eth =
  mkETH
    ethdma
    mac
    mmc
    ptp
    (do
        syscfgRCCEnable syscfg
        modifyReg (syscfgRegPMC syscfg) $
          -- Set RMII
          setBit syscfg_pmc_mii_rmii_sel
    )
    (\pin -> findAFByPin pin "ETH" afDB)

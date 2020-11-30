package io.choerodon.agile.infra.utils;

import org.hzero.core.base.BaseConstants;

import java.math.BigDecimal;
import java.util.Objects;

public class DataUtil {
    public static BigDecimal divide(BigDecimal divisor, BigDecimal dividend, int scale) {
        return Objects.isNull(dividend) || Objects.isNull(divisor) || dividend.compareTo(new BigDecimal(0)) == 0 ?
                new BigDecimal(0) :
                divisor.divide(dividend, scale, BigDecimal.ROUND_HALF_DOWN);
    }

    public static BigDecimal add(BigDecimal addend, BigDecimal augend) {
        addend = Objects.isNull(addend) ? new BigDecimal(0) : addend;
        augend = Objects.isNull(augend) ? new BigDecimal(0) : augend;
        return addend.add(augend);
    }

    public static BigDecimal multiply(BigDecimal multiplier, BigDecimal multiplicand) {
        return Objects.isNull(multiplier) || Objects.isNull(multiplicand) ? new BigDecimal(BaseConstants.Digital.ZERO) :
                multiplier.multiply(multiplicand);
    }
}

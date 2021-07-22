package io.choerodon.agile.api.vo.report;

import org.hzero.starter.keyencrypt.core.Encrypt;

import java.io.Serializable;
import java.util.List;

/**
 * @author chihao.ran@hand-china.com
 * 2021/06/11 11:30
 */
public class CustomChartDimensionVO implements Serializable {
    private static final long serialVersionUID = 4614823351557783967L;

    @Encrypt
    private Long comparedId;
    private List<CustomChartPointVO> pointList;
    private String comparedValue;
    private String comparedKey;

    public String getComparedValue() {
        return comparedValue;
    }

    public void setComparedValue(String comparedValue) {
        this.comparedValue = comparedValue;
    }

    public Long getComparedId() {
        return comparedId;
    }

    public void setComparedId(Long comparedId) {
        this.comparedId = comparedId;
    }

    public List<CustomChartPointVO> getPointList() {
        return pointList;
    }

    public void setPointList(List<CustomChartPointVO> pointList) {
        this.pointList = pointList;
    }

    public static long getSerialVersionUID() {
        return serialVersionUID;
    }

    public String getComparedKey() {
        return comparedKey;
    }

    public void setComparedKey(String comparedKey) {
        this.comparedKey = comparedKey;
    }
}

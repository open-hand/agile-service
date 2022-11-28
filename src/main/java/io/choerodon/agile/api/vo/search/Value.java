package io.choerodon.agile.api.vo.search;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

import io.swagger.annotations.ApiModelProperty;

import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author superlee
 * @since 2022-11-02
 */
public class Value {

    @ApiModelProperty("字符串快速搜索值")
    private String valueStr;
    @ApiModelProperty("字符串数组快速搜索值")
    private List<String> valueStrList;
    @ApiModelProperty("id快速搜索值")
    @Encrypt
    private Long valueId;
    @ApiModelProperty("id数组快速搜索值")
    @Encrypt
    private List<Long> valueIdList;
    @ApiModelProperty("未加密id")
    private Long noEncryptId;
    @ApiModelProperty("未加密id集合")
    private List<Long> noEncryptIdList;

    @ApiModelProperty("数字(允许小数)快速搜索值")
    private BigDecimal valueDecimal;
    @ApiModelProperty("时间快速搜索值")
    private Date valueDate;

    private String valueSpecial;

    private List<Object> objectList;

    public List<Object> getObjectList() {
        return objectList;
    }

    public void setObjectList(List<Object> objectList) {
        this.objectList = objectList;
    }

    public String getValueStr() {
        return valueStr;
    }

    public void setValueStr(String valueStr) {
        this.valueStr = valueStr;
    }

    public List<String> getValueStrList() {
        return valueStrList;
    }

    public void setValueStrList(List<String> valueStrList) {
        this.valueStrList = valueStrList;
    }

    public Long getValueId() {
        return valueId;
    }

    public void setValueId(Long valueId) {
        this.valueId = valueId;
    }

    public List<Long> getValueIdList() {
        return valueIdList;
    }

    public void setValueIdList(List<Long> valueIdList) {
        this.valueIdList = valueIdList;
    }

    public Long getNoEncryptId() {
        return noEncryptId;
    }

    public void setNoEncryptId(Long noEncryptId) {
        this.noEncryptId = noEncryptId;
    }

    public List<Long> getNoEncryptIdList() {
        return noEncryptIdList;
    }

    public void setNoEncryptIdList(List<Long> noEncryptIdList) {
        this.noEncryptIdList = noEncryptIdList;
    }

    public BigDecimal getValueDecimal() {
        return valueDecimal;
    }

    public void setValueDecimal(BigDecimal valueDecimal) {
        this.valueDecimal = valueDecimal;
    }

    public Date getValueDate() {
        return valueDate;
    }

    public void setValueDate(Date valueDate) {
        this.valueDate = valueDate;
    }

    public String getValueSpecial() {
        return valueSpecial;
    }

    public void setValueSpecial(String valueSpecial) {
        this.valueSpecial = valueSpecial;
    }

    @Override
    public String toString() {
        return "Value{" +
                "valueStr='" + valueStr + '\'' +
                ", valueStrList=" + valueStrList +
                ", valueId=" + valueId +
                ", valueIdList=" + valueIdList +
                ", noEncryptId=" + noEncryptId +
                ", noEncryptIdList=" + noEncryptIdList +
                ", valueDecimal=" + valueDecimal +
                ", valueDate=" + valueDate +
                ", valueSpecial='" + valueSpecial + '\'' +
                ", objectList=" + objectList +
                '}';
    }
}

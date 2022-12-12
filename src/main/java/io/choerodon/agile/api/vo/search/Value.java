package io.choerodon.agile.api.vo.search;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModelProperty;

import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author superlee
 * @since 2022-11-02
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class Value implements Serializable {
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
    @ApiModelProperty("特殊值")
    private String valueSpecial;
    @ApiModelProperty("对象列表")
    private List<? extends Object> objectList;
    @ApiModelProperty("工作台筛选项目下的工作类型")
    private List<IssueTypeProject> issueTypeProjectList;

    public List<IssueTypeProject> getIssueTypeProjectList() {
        return issueTypeProjectList;
    }

    public void setIssueTypeProjectList(List<IssueTypeProject> issueTypeProjectList) {
        this.issueTypeProjectList = issueTypeProjectList;
    }

    public List<? extends Object> getObjectList() {
        return objectList;
    }

    public Value setObjectList(List<? extends Object> objectList) {
        this.objectList = objectList;
        return this;
    }

    public String getValueStr() {
        return valueStr;
    }

    public Value setValueStr(String valueStr) {
        this.valueStr = valueStr;
        return this;
    }

    public List<String> getValueStrList() {
        return valueStrList;
    }

    public Value setValueStrList(List<String> valueStrList) {
        this.valueStrList = valueStrList;
        return this;
    }

    public Long getValueId() {
        return valueId;
    }

    public Value setValueId(Long valueId) {
        this.valueId = valueId;
        return this;
    }

    public List<Long> getValueIdList() {
        return valueIdList;
    }

    public Value setValueIdList(List<Long> valueIdList) {
        this.valueIdList = valueIdList;
        return this;
    }

    public Long getNoEncryptId() {
        return noEncryptId;
    }

    public Value setNoEncryptId(Long noEncryptId) {
        this.noEncryptId = noEncryptId;
        return this;
    }

    public List<Long> getNoEncryptIdList() {
        return noEncryptIdList;
    }

    public Value setNoEncryptIdList(List<Long> noEncryptIdList) {
        this.noEncryptIdList = noEncryptIdList;
        return this;
    }

    public BigDecimal getValueDecimal() {
        return valueDecimal;
    }

    public Value setValueDecimal(BigDecimal valueDecimal) {
        this.valueDecimal = valueDecimal;
        return this;
    }

    public Date getValueDate() {
        return valueDate;
    }

    public Value setValueDate(Date valueDate) {
        this.valueDate = valueDate;
        return this;
    }

    public String getValueSpecial() {
        return valueSpecial;
    }

    public Value setValueSpecial(String valueSpecial) {
        this.valueSpecial = valueSpecial;
        return this;
    }

    public static class IssueTypeProject {
        private Long projectId;
        @Encrypt
        private Long issueTypeId;

        public Long getProjectId() {
            return projectId;
        }

        public void setProjectId(Long projectId) {
            this.projectId = projectId;
        }

        public Long getIssueTypeId() {
            return issueTypeId;
        }

        public void setIssueTypeId(Long issueTypeId) {
            this.issueTypeId = issueTypeId;
        }

        @Override
        public String toString() {
            return "IssueTypeProject{" +
                    "projectId=" + projectId +
                    ", issueTypeId=" + issueTypeId +
                    '}';
        }
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
                ", issueTypeProjectList=" + issueTypeProjectList +
                '}';
    }
}

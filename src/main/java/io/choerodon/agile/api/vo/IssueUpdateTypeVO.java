package io.choerodon.agile.api.vo;


import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModelProperty;

import io.choerodon.agile.infra.utils.StringUtil;

import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author dinghuang123@gmail.com
 * @since 2018/6/5
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class IssueUpdateTypeVO {

    @ApiModelProperty(value = "问题主键id")
    @Encrypt
    private Long issueId;

    @ApiModelProperty(value = "史诗名称")
    private String epicName;

    @ApiModelProperty(value = "问题类型code")
    private String typeCode;

    @ApiModelProperty(value = "编号")
    private Long objectVersionNumber;

    @ApiModelProperty(value = "项目id")
    private Long projectId;

    @ApiModelProperty(value = "问题类型id")
    @Encrypt
    private Long issueTypeId;

    @ApiModelProperty(value = "类型编码")
    private String applyType;

    private BatchUpdateFieldsValueVo batchUpdateFieldsValueVo;

    public BatchUpdateFieldsValueVo getBatchUpdateFieldsValueVo() {
        return batchUpdateFieldsValueVo;
    }

    public IssueUpdateTypeVO setBatchUpdateFieldsValueVo(BatchUpdateFieldsValueVo batchUpdateFieldsValueVo) {
        this.batchUpdateFieldsValueVo = batchUpdateFieldsValueVo;
        return this;
    }

    public Long getIssueId() {
        return issueId;
    }

    public IssueUpdateTypeVO setIssueId(Long issueId) {
        this.issueId = issueId;
        return this;
    }

    public String getEpicName() {
        return epicName;
    }

    public IssueUpdateTypeVO setEpicName(String epicName) {
        this.epicName = epicName;
        return this;
    }

    public String getTypeCode() {
        return typeCode;
    }

    public IssueUpdateTypeVO setTypeCode(String typeCode) {
        this.typeCode = typeCode;
        return this;
    }

    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public IssueUpdateTypeVO setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
        return this;
    }

    public Long getProjectId() {
        return projectId;
    }

    public IssueUpdateTypeVO setProjectId(Long projectId) {
        this.projectId = projectId;
        return this;
    }

    public IssueUpdateTypeVO setIssueTypeId(Long issueTypeId) {
        this.issueTypeId = issueTypeId;
        return this;
    }

    public Long getIssueTypeId() {
        return issueTypeId;
    }

    public String getApplyType() {
        return applyType;
    }

    public IssueUpdateTypeVO setApplyType(String applyType) {
        this.applyType = applyType;
        return this;
    }

    @Override
    public String toString() {
        return StringUtil.getToString(this);
    }
}

package io.choerodon.agile.api.vo;


import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;
import java.util.Set;

/**
 * @author superlee
 * @since 2020-08-10
 */
public class PageConfigUpdateVO {
    @ApiModelProperty(value = "问题类型")
    private String issueType;
    @ApiModelProperty(value = "字段")
    private List<PageConfigFieldVO> fields;
    @ApiModelProperty(value = "问题类型字段")
    private IssueTypeFieldVO issueTypeFieldVO;

    @Encrypt
    @ApiModelProperty(value = "删除的id")
    private Set<Long> deleteIds;
    @ApiModelProperty(value = "创建页面字段")
    private List<ObjectSchemeFieldCreateVO> createdFields;
    @ApiModelProperty(value = "添加的字段")
    private List<PageConfigFieldVO> addFields;

    @Encrypt
    @ApiModelProperty(value = "问题类型")
    private Long issueTypeId;

    public List<PageConfigFieldVO> getAddFields() {
        return addFields;
    }

    public void setAddFields(List<PageConfigFieldVO> addFields) {
        this.addFields = addFields;
    }

    public List<ObjectSchemeFieldCreateVO> getCreatedFields() {
        return createdFields;
    }

    public void setCreatedFields(List<ObjectSchemeFieldCreateVO> createdFields) {
        this.createdFields = createdFields;
    }

    public Set<Long> getDeleteIds() {
        return deleteIds;
    }

    public void setDeleteIds(Set<Long> deleteIds) {
        this.deleteIds = deleteIds;
    }

    public IssueTypeFieldVO getIssueTypeFieldVO() {
        return issueTypeFieldVO;
    }

    public void setIssueTypeFieldVO(IssueTypeFieldVO issueTypeFieldVO) {
        this.issueTypeFieldVO = issueTypeFieldVO;
    }

    public String getIssueType() {
        return issueType;
    }

    public void setIssueType(String issueType) {
        this.issueType = issueType;
    }

    public List<PageConfigFieldVO> getFields() {
        return fields;
    }

    public void setFields(List<PageConfigFieldVO> fields) {
        this.fields = fields;
    }

    public Long getIssueTypeId() {
        return issueTypeId;
    }

    public void setIssueTypeId(Long issueTypeId) {
        this.issueTypeId = issueTypeId;
    }
}

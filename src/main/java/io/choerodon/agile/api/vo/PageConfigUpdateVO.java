package io.choerodon.agile.api.vo;


import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;
import java.util.Set;

/**
 * @author superlee
 * @since 2020-08-10
 */
public class PageConfigUpdateVO {

    private String issueType;

    private List<PageConfigFieldVO> fields;

    private IssueTypeFieldVO issueTypeFieldVO;

    @Encrypt
    private Set<Long> deleteIds;

    private List<ObjectSchemeFieldCreateVO> createdFields;

    private List<PageConfigFieldVO> addFields;

    @Encrypt
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

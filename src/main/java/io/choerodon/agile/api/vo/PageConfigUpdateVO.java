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

    private List<Field> fields;

    private IssueTypeFieldVO issueTypeFieldVO;

    @Encrypt
    private Set<Long> deleteIds;

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

    public List<Field> getFields() {
        return fields;
    }

    public void setFields(List<Field> fields) {
        this.fields = fields;
    }

    public static class Field {
        @Encrypt
        private Long fieldId;

        private Boolean required;

        private Boolean created;

        private Boolean edited;

        private Long objectVersionNumber;

        public Long getObjectVersionNumber() {
            return objectVersionNumber;
        }

        public void setObjectVersionNumber(Long objectVersionNumber) {
            this.objectVersionNumber = objectVersionNumber;
        }

        public Long getFieldId() {
            return fieldId;
        }

        public void setFieldId(Long fieldId) {
            this.fieldId = fieldId;
        }

        public Boolean getRequired() {
            return required;
        }

        public void setRequired(Boolean required) {
            this.required = required;
        }

        public Boolean getCreated() {
            return created;
        }

        public void setCreated(Boolean created) {
            this.created = created;
        }

        public Boolean getEdited() {
            return edited;
        }

        public void setEdited(Boolean edited) {
            this.edited = edited;
        }
    }

}

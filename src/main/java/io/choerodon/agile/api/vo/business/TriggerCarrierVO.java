package io.choerodon.agile.api.vo.business;

import io.choerodon.mybatis.domain.AuditDomain;

import java.util.List;
import java.util.Set;

/**
 * @author superlee
 * @since 2021-08-23
 */
public class TriggerCarrierVO {

    private Long projectId;

    private Long instanceId;

    private Set<Long> memberFieldIds;

    private List<String> fieldList;

    private List<String> executedRules;

    private Long issueTypeId;

    private boolean checkMode;

    private AuditDomain auditDomain;

    private Long noticeInstanceId;

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public Long getInstanceId() {
        return instanceId;
    }

    public void setInstanceId(Long instanceId) {
        this.instanceId = instanceId;
    }

    public Set<Long> getMemberFieldIds() {
        return memberFieldIds;
    }

    public void setMemberFieldIds(Set<Long> memberFieldIds) {
        this.memberFieldIds = memberFieldIds;
    }

    public List<String> getFieldList() {
        return fieldList;
    }

    public void setFieldList(List<String> fieldList) {
        this.fieldList = fieldList;
    }

    public List<String> getExecutedRules() {
        return executedRules;
    }

    public void setExecutedRules(List<String> executedRules) {
        this.executedRules = executedRules;
    }

    public Long getIssueTypeId() {
        return issueTypeId;
    }

    public void setIssueTypeId(Long issueTypeId) {
        this.issueTypeId = issueTypeId;
    }

    public boolean isCheckMode() {
        return checkMode;
    }

    public void setCheckMode(boolean checkMode) {
        this.checkMode = checkMode;
    }

    public AuditDomain getAuditDomain() {
        return auditDomain;
    }

    public void setAuditDomain(AuditDomain auditDomain) {
        this.auditDomain = auditDomain;
    }

    public Long getNoticeInstanceId() {
        return noticeInstanceId;
    }

    public void setNoticeInstanceId(Long noticeInstanceId) {
        this.noticeInstanceId = noticeInstanceId;
    }
}

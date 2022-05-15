package io.choerodon.agile.api.vo.business;

import io.choerodon.mybatis.domain.AuditDomain;
import io.swagger.annotations.ApiModelProperty;

import java.util.List;
import java.util.Set;

/**
 * @author superlee
 * @since 2021-08-23
 */
public class TriggerCarrierVO {
    @ApiModelProperty(value = "项目id")
    private Long projectId;
    @ApiModelProperty(value = "实例id")
    private Long instanceId;
    @ApiModelProperty(value = "成员字段id")
    private Set<Long> memberFieldIds;
    @ApiModelProperty(value = "字段编码集合")
    private List<String> fieldList;
    @ApiModelProperty(value = "执行过的规则")
    private List<String> executedRules;
    @ApiModelProperty(value = "问题类型id")
    private Long issueTypeId;
    @ApiModelProperty(value = "检查模式")
    private boolean checkMode;
    @ApiModelProperty(value = "审计域")
    private AuditDomain auditDomain;
    @ApiModelProperty(value = "通知实例id")
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

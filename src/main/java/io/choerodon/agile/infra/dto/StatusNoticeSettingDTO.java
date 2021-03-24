package io.choerodon.agile.infra.dto;

import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonInclude;

import io.choerodon.agile.api.vo.StatusNoticeSettingVO;
import io.choerodon.agile.infra.enums.StatusNoticeUserType;
import io.choerodon.mybatis.domain.AuditDomain;
import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import org.hzero.core.base.BaseConstants;

/**
 * 邮件通知
 *
 * @author choerodon@choerodon.cn 2020-08-12 11:41:01
 */
@ApiModel("邮件通知")
@VersionAudit
@ModifyAudit
@JsonInclude(value = JsonInclude.Include.NON_NULL)
@Table(name = "fd_status_notice_setting")
public class StatusNoticeSettingDTO extends AuditDomain {

    public static final String FIELD_STATUS_ID = "statusId";
    public static final String FIELD_ISSUE_TYPE_ID = "issueTypeId";
    public static final String FIELD_PROJECT_ID = "projectId";
    public static final String FIELD_ORGANIZATION_ID = "organizationId";

    public StatusNoticeSettingDTO() {
    }

    public StatusNoticeSettingDTO(Long projectId, Long issueTypeId, Long statusId) {
        this.issueTypeId = issueTypeId;
        this.projectId = projectId;
        this.statusId = statusId;
    }

    public StatusNoticeSettingDTO(StatusNoticeSettingVO statusNoticeSettingVO, String userType) {
        this.issueTypeId = statusNoticeSettingVO.getIssueTypeId();
        this.projectId = statusNoticeSettingVO.getProjectId();
        this.statusId = statusNoticeSettingVO.getStatusId();
        this.organizationId = statusNoticeSettingVO.getOrganizationId();
        this.noticeType = String.join(BaseConstants.Symbol.COMMA, statusNoticeSettingVO.getNoticeTypeList());
        this.userType = userType;
    }

    public StatusNoticeSettingDTO(StatusNoticeSettingVO statusNoticeSettingVO, Long userId) {
        this.issueTypeId = statusNoticeSettingVO.getIssueTypeId();
        this.projectId = statusNoticeSettingVO.getProjectId();
        this.statusId = statusNoticeSettingVO.getStatusId();
        this.noticeType = String.join(BaseConstants.Symbol.COMMA, statusNoticeSettingVO.getNoticeTypeList());
        this.userType = StatusNoticeUserType.SPECIFIER;
        this.userId = userId;
    }

    @ApiModelProperty("主键id")
    @Id
    @GeneratedValue
    private Long id;

    @ApiModelProperty(value = "issue类型Id",required = true)
    private Long issueTypeId;

    @ApiModelProperty(value = "项目id",required = true)
    private Long projectId;

    @ApiModelProperty(value = "状态id",required = true)
    private Long statusId;

    @ApiModelProperty(value = "用户类型，基本类型projectOwner, assignee, reporter, specifier",required = true)
    private String userType;

    @ApiModelProperty(value = "用户id")
    private Long userId;

    @ApiModelProperty(value = "通知类型",required = true)
    private String noticeType;

    private Long organizationId;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getIssueTypeId() {
        return issueTypeId;
    }

    public void setIssueTypeId(Long issueTypeId) {
        this.issueTypeId = issueTypeId;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public Long getStatusId() {
        return statusId;
    }

    public void setStatusId(Long statusId) {
        this.statusId = statusId;
    }

    public String getUserType() {
        return userType;
    }

    public void setUserType(String userType) {
        this.userType = userType;
    }

    public Long getUserId() {
        return userId;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
    }
    /**
     * @return projectOwner, assignee, reporter, specifier
     */
    public String getNoticeType() {
        return noticeType;
    }

    public void setNoticeType(String noticeType) {
        this.noticeType = noticeType;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }
}

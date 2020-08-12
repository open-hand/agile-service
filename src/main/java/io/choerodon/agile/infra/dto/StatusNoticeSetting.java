package io.choerodon.agile.infra.dto;

import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;

import com.fasterxml.jackson.annotation.JsonInclude;

import javax.validation.constraints.NotBlank;
import io.choerodon.mybatis.domain.AuditDomain;
import io.choerodon.mybatis.annotation.ModifyAudit;
import io.choerodon.mybatis.annotation.VersionAudit;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;

/**
 * 邮件通知
 *
 * @author choerodon@choerodon.cn 2020-08-12 11:41:01
 */
@ApiModel("邮件通知")
@VersionAudit
@ModifyAudit
@JsonInclude(value = JsonInclude.Include.NON_NULL)
@Table(name = "fd_ status_notice_setting")
public class StatusNoticeSetting extends AuditDomain {

    public static final String FIELD_ID = "id";
    public static final String FIELD_ISSUE_TYPE_ID = "issueTypeId";
    public static final String FIELD_PROJECT_ID = "projectId";
    public static final String FIELD_STATUS_ID = "statusId";
    public static final String FIELD_USER_TYPE = "userType";
    public static final String FIELD_USER_ID = "userId";
    public static final String FIELD_NOTICE_TYPE = "noticeType";

    //
    // 业务方法(按public protected private顺序排列)
    // ------------------------------------------------------------------------------

    //
    // 数据库字段
    // ------------------------------------------------------------------------------


    @ApiModelProperty("")
    @Id
    @GeneratedValue
    private Long id;
    @ApiModelProperty(value = "",required = true)
    @NotNull
    private Long issueTypeId;
    @ApiModelProperty(value = "",required = true)
    @NotNull
    private Long projectId;
    @ApiModelProperty(value = "",required = true)
    @NotNull
    private Long statusId;
    @ApiModelProperty(value = "",required = true)
    @NotBlank
    private String userType;
   @ApiModelProperty(value = "")
    private Long userId;
    @ApiModelProperty(value = "projectOwner, assignee, reporter, specifier",required = true)
    @NotBlank
    private String noticeType;

	//
    // 非数据库字段
    // ------------------------------------------------------------------------------

    //
    // getter/setter
    // ------------------------------------------------------------------------------

    /**
     * @return
     */
	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}
    /**
     * @return
     */
	public Long getIssueTypeId() {
		return issueTypeId;
	}

	public void setIssueTypeId(Long issueTypeId) {
		this.issueTypeId = issueTypeId;
	}
    /**
     * @return
     */
	public Long getProjectId() {
		return projectId;
	}

	public void setProjectId(Long projectId) {
		this.projectId = projectId;
	}
    /**
     * @return
     */
	public Long getStatusId() {
		return statusId;
	}

	public void setStatusId(Long statusId) {
		this.statusId = statusId;
	}
    /**
     * @return
     */
	public String getUserType() {
		return userType;
	}

	public void setUserType(String userType) {
		this.userType = userType;
	}
    /**
     * @return
     */
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

}

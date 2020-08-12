package io.choerodon.agile.api.vo;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import javax.validation.constraints.NotNull;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.choerodon.agile.infra.enums.StatusNoticeUserType;
import io.choerodon.mybatis.domain.AuditDomain;
import io.swagger.annotations.ApiModelProperty;
import org.apache.commons.lang3.StringUtils;
import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author jiaxu.cui@hand-china.com 2020/8/12 下午1:59
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class StatusNoticeSettingVO extends AuditDomain {

    public StatusNoticeSettingVO(Long issueTypeId, Long projectId, Long statusId) {
        this.issueTypeId = issueTypeId;
        this.projectId = projectId;
        this.statusId = statusId;
    }

    public void addUserWithNotice(String userType, Long userId){
        if (StringUtils.equals(StatusNoticeUserType.SPECIFIER.getCode(), userType)){
            userIdList.add(userId);
        }else {
            userTypeList.add(userType);
        }
    }

    @ApiModelProperty(value = "issue类型Id",required = true)
    @NotNull
    private Long issueTypeId;
    @ApiModelProperty(value = "项目id",required = true)
    @NotNull
    private Long projectId;
    @ApiModelProperty(value = "状态id",required = true)
    @NotNull
    private Long statusId;
    @ApiModelProperty(value = "用户类型，projectOwner, assignee, reporter, specifier")
    private List<String> userTypeList = new ArrayList<>();
    @ApiModelProperty(value = "用户id")
    @Encrypt
    private List<Long> userIdList = new ArrayList<>();
    @ApiModelProperty(value = "通知类型")
    private String noticeType;

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

    public List<String> getUserTypeList() {
        return userTypeList;
    }

    public void setUserTypeList(List<String> userTypeList) {
        this.userTypeList = userTypeList;
    }

    public List<Long> getUserIdList() {
        return userIdList;
    }

    public void setUserIdList(List<Long> userIdList) {
        this.userIdList = userIdList;
    }

    public String getNoticeType() {
        return noticeType;
    }

    public void setNoticeType(String noticeType) {
        this.noticeType = noticeType;
    }
}

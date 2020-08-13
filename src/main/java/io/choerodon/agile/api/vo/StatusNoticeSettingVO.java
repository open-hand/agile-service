package io.choerodon.agile.api.vo;

import java.util.*;

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
        if (StringUtils.equals(StatusNoticeUserType.SPECIFIER, userType)){
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
    private Set<String> userTypeList = new HashSet<>();
    @ApiModelProperty(value = "用户id")
    @Encrypt
    private Set<Long> userIdList = new HashSet<>();
    @ApiModelProperty(value = "通知类型")
    private List<String> noticeTypeList;

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

    public Set<String> getUserTypeList() {
        return userTypeList;
    }

    public void setUserTypeList(Set<String> userTypeList) {
        this.userTypeList = userTypeList;
    }

    public Set<Long> getUserIdList() {
        return userIdList;
    }

    public void setUserIdList(Set<Long> userIdList) {
        this.userIdList = userIdList;
    }

    public List<String> getNoticeTypeList() {
        return noticeTypeList;
    }

    public void setNoticeTypeList(List<String> noticeTypeList) {
        this.noticeTypeList = noticeTypeList;
    }
}

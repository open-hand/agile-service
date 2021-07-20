package io.choerodon.agile.api.vo.business;


import io.swagger.annotations.ApiModelProperty;

import java.util.Date;
import java.util.List;

import io.choerodon.agile.api.vo.UserVO;

/**
 * @author chihao.ran@hand-china.com
 * 2021/07/20 15:01
 */
public class ProjectRelationshipInfoVO {

    @ApiModelProperty("主键Id")
    private Long id;
    @ApiModelProperty("项目Id")
    private Long projectId;
    @ApiModelProperty("项目组的项目Id")
    private Long parentId;
    @ApiModelProperty("开始时间")
    private Date startDate;
    @ApiModelProperty("结束时间")
    private Date endDate;
    @ApiModelProperty("是否启用")
    private Boolean enabled;
    @ApiModelProperty("所属ProgramId")
    private Long programId;
    @ApiModelProperty("项目Code")
    private String projCode;
    @ApiModelProperty("项目Name")
    private String projName;
    @ApiModelProperty("项目人员总数")
    private Integer userCount;
    @ApiModelProperty("项目成员")
    private List<UserVO> userDTOS;
    @ApiModelProperty("项目群关系创建时间")
    private Date creationDate;
    @ApiModelProperty("负责人id")
    private Long responsibilityUserId;
    @ApiModelProperty("负责人")
    private UserVO responsibilityUser;
    @ApiModelProperty(value = "版本号")
    private Long objectVersionNumber;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public Long getParentId() {
        return parentId;
    }

    public void setParentId(Long parentId) {
        this.parentId = parentId;
    }

    public Date getStartDate() {
        return startDate;
    }

    public void setStartDate(Date startDate) {
        this.startDate = startDate;
    }

    public Date getEndDate() {
        return endDate;
    }

    public void setEndDate(Date endDate) {
        this.endDate = endDate;
    }

    public Boolean getEnabled() {
        return enabled;
    }

    public void setEnabled(Boolean enabled) {
        this.enabled = enabled;
    }

    public Long getProgramId() {
        return programId;
    }

    public void setProgramId(Long programId) {
        this.programId = programId;
    }

    public String getProjCode() {
        return projCode;
    }

    public void setProjCode(String projCode) {
        this.projCode = projCode;
    }

    public String getProjName() {
        return projName;
    }

    public void setProjName(String projName) {
        this.projName = projName;
    }

    public Integer getUserCount() {
        return userCount;
    }

    public void setUserCount(Integer userCount) {
        this.userCount = userCount;
    }

    public List<UserVO> getUserDTOS() {
        return this.userDTOS;
    }

    public void setUserDTOS(List<UserVO> userDTOS) {
        this.userDTOS = userDTOS;
    }

    public Date getCreationDate() {
        return creationDate;
    }

    public void setCreationDate(Date creationDate) {
        this.creationDate = creationDate;
    }

    public Long getResponsibilityUserId() {
        return responsibilityUserId;
    }

    public void setResponsibilityUserId(Long responsibilityUserId) {
        this.responsibilityUserId = responsibilityUserId;
    }

    public UserVO getResponsibilityUser() {
        return responsibilityUser;
    }

    public void setResponsibilityUser(UserVO responsibilityUser) {
        this.responsibilityUser = responsibilityUser;
    }

    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public void setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
    }
}

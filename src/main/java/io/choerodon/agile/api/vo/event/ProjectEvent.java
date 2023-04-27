package io.choerodon.agile.api.vo.event;

import java.util.List;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/5/22.
 * Email: fuqianghuang01@gmail.com
 */
public class ProjectEvent {

    private Long projectId;

    private String projectCode;

    private String projectName;

    private Long organizationId;

    private String organizationCode;

    private String organizationName;

    private String userName;

    private Long userId;

    private String projectCategory;

    private List<String> roleLabels;

    private Long programId;

    private List<ProjectEventCategory> projectCategoryVOS;

    private List<ProjectEventCategory> newProjectCategoryVOS;

    private Boolean useTemplate;

    private Long workGroupId;

    private Long fromTemplateId;

    public List<ProjectEventCategory> getProjectCategoryVOS() {
        return projectCategoryVOS;
    }

    public void setProjectCategoryVOS(List<ProjectEventCategory> projectCategoryVOS) {
        this.projectCategoryVOS = projectCategoryVOS;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public String getProjectCode() {
        return projectCode;
    }

    public void setProjectCode(String projectCode) {
        this.projectCode = projectCode;
    }

    public String getProjectName() {
        return projectName;
    }

    public void setProjectName(String projectName) {
        this.projectName = projectName;
    }

    public String getOrganizationCode() {
        return organizationCode;
    }

    public void setOrganizationCode(String organizationCode) {
        this.organizationCode = organizationCode;
    }

    public String getOrganizationName() {
        return organizationName;
    }

    public void setOrganizationName(String organizationName) {
        this.organizationName = organizationName;
    }

    public String getUserName() {
        return userName;
    }

    public void setUserName(String userName) {
        this.userName = userName;
    }

    public Long getUserId() {
        return userId;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
    }

    public List<String> getRoleLabels() {
        return roleLabels;
    }

    public void setRoleLabels(List<String> roleLabels) {
        this.roleLabels = roleLabels;
    }

    public void setProjectCategory(String projectCategory) {
        this.projectCategory = projectCategory;
    }

    public String getProjectCategory() {
        return projectCategory;
    }

    public void setProgramId(Long programId) {
        this.programId = programId;
    }

    public Long getProgramId() {
        return programId;
    }

    public Boolean getUseTemplate() {
        return useTemplate;
    }

    public void setUseTemplate(Boolean useTemplate) {
        this.useTemplate = useTemplate;
    }

    public List<ProjectEventCategory> getNewProjectCategoryVOS() {
        return newProjectCategoryVOS;
    }

    public void setNewProjectCategoryVOS(List<ProjectEventCategory> newProjectCategoryVOS) {
        this.newProjectCategoryVOS = newProjectCategoryVOS;
    }

    public Long getWorkGroupId() {
        return workGroupId;
    }

    public void setWorkGroupId(Long workGroupId) {
        this.workGroupId = workGroupId;
    }

    public Long getOrganizationId() {
        return organizationId;
    }

    public void setOrganizationId(Long organizationId) {
        this.organizationId = organizationId;
    }

    public Long getFromTemplateId() {
        return fromTemplateId;
    }

    public void setFromTemplateId(Long fromTemplateId) {
        this.fromTemplateId = fromTemplateId;
    }
}

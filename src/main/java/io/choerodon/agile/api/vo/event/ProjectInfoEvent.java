package io.choerodon.agile.api.vo.event;

/**
 * @author superlee
 * @since 2022-08-19
 */
public class ProjectInfoEvent {

    private Long infoId;

    private Long projectId;

    private String projectCode;

    private String originProjectCode;

    public Long getInfoId() {
        return infoId;
    }

    public void setInfoId(Long infoId) {
        this.infoId = infoId;
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

    public String getOriginProjectCode() {
        return originProjectCode;
    }

    public void setOriginProjectCode(String originProjectCode) {
        this.originProjectCode = originProjectCode;
    }
}

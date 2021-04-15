package io.choerodon.agile.api.vo;

import com.fasterxml.jackson.annotation.JsonIgnore;
import io.choerodon.agile.infra.dto.PublishVersionDTO;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.util.List;
import java.util.Objects;

/**
 * @author superlee
 * @since 2021-03-25
 */
public class TagVO {

    @NotEmpty(message = "error.tag.app.service.code")
    private String appServiceCode;
    @NotEmpty(message = "error.tag.name")
    private String tagName;
    @NotNull(message = "error.tag.project.id")
    private Long projectId;

    private String appServiceName;

    @JsonIgnore
    private List<PublishVersionDTO> publishVersionList;

    public List<PublishVersionDTO> getPublishVersionList() {
        return publishVersionList;
    }

    public void setPublishVersionList(List<PublishVersionDTO> publishVersionList) {
        this.publishVersionList = publishVersionList;
    }

    public String getAppServiceCode() {
        return appServiceCode;
    }

    public void setAppServiceCode(String appServiceCode) {
        this.appServiceCode = appServiceCode;
    }

    public String getTagName() {
        return tagName;
    }

    public void setTagName(String tagName) {
        this.tagName = tagName;
    }

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public String getAppServiceName() {
        return appServiceName;
    }

    public void setAppServiceName(String appServiceName) {
        this.appServiceName = appServiceName;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof TagVO)) return false;
        TagVO tagVO = (TagVO) o;
        return Objects.equals(getAppServiceCode(), tagVO.getAppServiceCode()) &&
                Objects.equals(getTagName(), tagVO.getTagName()) &&
                Objects.equals(getProjectId(), tagVO.getProjectId());
    }

    @Override
    public int hashCode() {
        return Objects.hash(getAppServiceCode(), getTagName(), getProjectId());
    }
}

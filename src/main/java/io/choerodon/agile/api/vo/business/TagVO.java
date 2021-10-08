package io.choerodon.agile.api.vo.business;

import org.hzero.starter.keyencrypt.core.Encrypt;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
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

    private Long objectVersionNumber;

    @Encrypt
    private Long id;

    private String alias;

    @Encrypt
    private Long issueId;

    public String getAlias() {
        return alias;
    }

    public void setAlias(String alias) {
        this.alias = alias;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getObjectVersionNumber() {
        return objectVersionNumber;
    }

    public void setObjectVersionNumber(Long objectVersionNumber) {
        this.objectVersionNumber = objectVersionNumber;
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

    public Long getIssueId() {
        return issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
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

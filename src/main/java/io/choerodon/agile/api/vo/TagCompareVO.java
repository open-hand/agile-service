package io.choerodon.agile.api.vo;

import org.hzero.starter.keyencrypt.core.Encrypt;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

/**
 * @author superlee
 * @since 2021-04-15
 */
public class TagCompareVO {

    @NotNull(message = "error.projectId.null")
    private Long projectId;
    @Encrypt
    @NotNull(message = "error.appServiceId.null")
    private Long appServiceId;
    @NotEmpty(message = "error.appServiceCode.empty")
    private String appServiceCode;
    @NotEmpty(message = "error.sourceTag.empty")
    private String sourceTag;

    private String targetTag;

    public Long getProjectId() {
        return projectId;
    }

    public void setProjectId(Long projectId) {
        this.projectId = projectId;
    }

    public Long getAppServiceId() {
        return appServiceId;
    }

    public void setAppServiceId(Long appServiceId) {
        this.appServiceId = appServiceId;
    }

    public String getSourceTag() {
        return sourceTag;
    }

    public void setSourceTag(String sourceTag) {
        this.sourceTag = sourceTag;
    }

    public String getTargetTag() {
        return targetTag;
    }

    public void setTargetTag(String targetTag) {
        this.targetTag = targetTag;
    }

    public String getAppServiceCode() {
        return appServiceCode;
    }

    public void setAppServiceCode(String appServiceCode) {
        this.appServiceCode = appServiceCode;
    }
}

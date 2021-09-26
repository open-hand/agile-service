package io.choerodon.agile.api.vo;

import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author superlee
 * @since 2021-09-26
 */
public class IssueFeatureVO {

    @Encrypt
    private Long issueId;

    private String featureName;

    private String featureColor;

    public Long getIssueId() {
        return issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
    }

    public String getFeatureName() {
        return featureName;
    }

    public void setFeatureName(String featureName) {
        this.featureName = featureName;
    }

    public String getFeatureColor() {
        return featureColor;
    }

    public void setFeatureColor(String featureColor) {
        this.featureColor = featureColor;
    }
}

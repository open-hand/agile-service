package io.choerodon.agile.api.vo;

import com.alibaba.fastjson.JSONObject;

import java.util.List;

/**
 * @author zhaotianxin
 * @date 2021-07-15 20:35
 */
public class CopyIssueRequiredFieldVO {

    private Long issueId;

    private JSONObject predefinedFields;

    private List<PageFieldViewCreateVO> customFields;

    public Long getIssueId() {
        return issueId;
    }

    public void setIssueId(Long issueId) {
        this.issueId = issueId;
    }

    public JSONObject getPredefinedFields() {
        return predefinedFields;
    }

    public void setPredefinedFields(JSONObject predefinedFields) {
        this.predefinedFields = predefinedFields;
    }

    public List<PageFieldViewCreateVO> getCustomFields() {
        return customFields;
    }

    public void setCustomFields(List<PageFieldViewCreateVO> customFields) {
        this.customFields = customFields;
    }
}

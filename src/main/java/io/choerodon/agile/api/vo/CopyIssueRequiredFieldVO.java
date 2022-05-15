package io.choerodon.agile.api.vo;

import com.alibaba.fastjson.JSONObject;
import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;

/**
 * @author zhaotianxin
 * @date 2021-07-15 20:35
 */
public class CopyIssueRequiredFieldVO {

    @Encrypt
    @ApiModelProperty("问题id")
    private Long issueId;
    @ApiModelProperty("预定义字段")
    private JSONObject predefinedFields;
    @ApiModelProperty("自定义字段")
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

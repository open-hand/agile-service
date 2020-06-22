package io.choerodon.agile.api.vo;

import com.alibaba.fastjson.JSONObject;
import io.choerodon.agile.infra.constants.EncryptionConstant;
import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;

/**
 * @author zhaotianxin
 * @date 2020-04-30 11:39
 */
public class BatchUpdateFieldsValueVo {

    @ApiModelProperty(value = "操作的issueId")
    @Encrypt/*(EncryptionConstant.AGILE_ISSUE)*/
    private List<Long> issueIds;

    @ApiModelProperty(value = "要修改的预定义字段的值")
    private JSONObject predefinedFields;

    @ApiModelProperty(value = "要修改的自定义字段的值")
    private List<PageFieldViewUpdateVO> customFields;

    public List<Long> getIssueIds() {
        return issueIds;
    }

    public void setIssueIds(List<Long> issueIds) {
        this.issueIds = issueIds;
    }

    public JSONObject getPredefinedFields() {
        return predefinedFields;
    }

    public void setPredefinedFields(JSONObject predefinedFields) {
        this.predefinedFields = predefinedFields;
    }

    public List<PageFieldViewUpdateVO> getCustomFields() {
        return customFields;
    }

    public void setCustomFields(List<PageFieldViewUpdateVO> customFields) {
        this.customFields = customFields;
    }
}

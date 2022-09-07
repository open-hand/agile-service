package io.choerodon.agile.api.vo;

import io.choerodon.agile.infra.utils.StringUtil;
import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;

/**
 * @author dinghuang123@gmail.com
 * @since 2018/7/6
 */
public class CopyConditionVO {

    @ApiModelProperty(value = "问题概要")
    private String summary;

    @ApiModelProperty(value = "是否复制子任务")
    private Boolean subTask;

    @ApiModelProperty(value = "是否复制问题链接")
    private Boolean issueLink;

    @Encrypt
    @ApiModelProperty(value = "需要复制的自定义字段")
    private List<Long> customFieldIds;

    @ApiModelProperty(value = "需要复制的预定义字段")
    private List<String> predefinedFieldNames;
    @ApiModelProperty("史诗名称")
    private String epicName;
    @ApiModelProperty("复制问题必填字段集合")
    private List<CopyIssueRequiredFieldVO> copyIssueRequiredFieldVOS;
    @ApiModelProperty("关联内容")
    private List<String> linkContents;

    public String getEpicName() {
        return epicName;
    }

    public void setEpicName(String epicName) {
        this.epicName = epicName;
    }

    public String getSummary() {
        return summary;
    }

    public void setSummary(String summary) {
        this.summary = summary;
    }

    public Boolean getSubTask() {
        return subTask;
    }

    public void setSubTask(Boolean subTask) {
        this.subTask = subTask;
    }

    public Boolean getIssueLink() {
        return issueLink;
    }

    public void setIssueLink(Boolean issueLink) {
        this.issueLink = issueLink;
    }

    @Override
    public String toString() {
        return StringUtil.getToString(this);
    }

    public List<Long> getCustomFieldIds() {
        return customFieldIds;
    }

    public void setCustomFieldIds(List<Long> customFieldIds) {
        this.customFieldIds = customFieldIds;
    }

    public List<String> getPredefinedFieldNames() {
        return predefinedFieldNames;
    }

    public void setPredefinedFieldNames(List<String> predefinedFieldNames) {
        this.predefinedFieldNames = predefinedFieldNames;
    }

    public List<CopyIssueRequiredFieldVO> getCopyIssueRequiredFieldVOS() {
        return copyIssueRequiredFieldVOS;
    }

    public void setCopyIssueRequiredFieldVOS(List<CopyIssueRequiredFieldVO> copyIssueRequiredFieldVOS) {
        this.copyIssueRequiredFieldVOS = copyIssueRequiredFieldVOS;
    }

    public List<String> getLinkContents() {
        return linkContents;
    }

    public void setLinkContents(List<String> linkContents) {
        this.linkContents = linkContents;
    }
}

package io.choerodon.agile.api.vo;

import com.fasterxml.jackson.annotation.JsonIgnore;
import io.choerodon.agile.infra.utils.StringUtil;
import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;
import java.util.Map;

/**
 * @author dinghuang123@gmail.com
 * @since 2018/5/31
 */
public class SearchVO {
    @ApiModelProperty(value = "导出字段")
    private List<String> exportFieldCodes;
    /**
     * 输入查询参数
     */
    @ApiModelProperty(value = "搜索参数")
    private Map<String, Object> searchArgs;

    /**
     * 过滤查询参数
     */
    @ApiModelProperty(value = "搜索参数")
    private Map<String, Object> advancedSearchArgs;

    /**
     * 关联查询参数
     */
    @ApiModelProperty(value = "搜索参数")
    private Map<String, Object> otherArgs;
    @Encrypt
    @ApiModelProperty(value = "快查id")
    private List<Long> quickFilterIds;
    @ApiModelProperty(value = "经办人过滤id")
    private List<Long> assigneeFilterIds;
    @ApiModelProperty(value = "仅故事")
    private Boolean onlyStory;
    @ApiModelProperty(value = "内容")
    private String content;
    @ApiModelProperty(value = "内容")
    private List<String> contents;
    @ApiModelProperty(value = "甘特图默认排序")
    private Boolean ganttDefaultOrder = false;
    @ApiModelProperty(value = "应用类型")
    private List<String> applyTypes;
    @ApiModelProperty(value = "展示的字段")
    private List<ObjectSchemeFieldVO> displayFields;
    @ApiModelProperty(value = "仅未完成的")
    private Boolean onlyInCompleted;

    public Boolean getOnlyInCompleted() {
        return onlyInCompleted;
    }

    public void setOnlyInCompleted(Boolean onlyInCompleted) {
        this.onlyInCompleted = onlyInCompleted;
    }

    public List<ObjectSchemeFieldVO> getDisplayFields() {
        return displayFields;
    }

    public void setDisplayFields(List<ObjectSchemeFieldVO> displayFields) {
        this.displayFields = displayFields;
    }

    public Boolean getGanttDefaultOrder() {
        return ganttDefaultOrder;
    }
    @JsonIgnore
    public void setGanttDefaultOrder(Boolean ganttDefaultOrder) {
        this.ganttDefaultOrder = ganttDefaultOrder;
    }

    public List<String> getExportFieldCodes() {
        return exportFieldCodes;
    }

    public void setExportFieldCodes(List<String> exportFieldCodes) {
        this.exportFieldCodes = exportFieldCodes;
    }

    public List<String> getContents() {
        return contents;
    }

    public void setContents(List<String> contents) {
        this.contents = contents;
    }

    public Boolean getOnlyStory() {
        return onlyStory;
    }

    public void setOnlyStory(Boolean onlyStory) {
        this.onlyStory = onlyStory;
    }

    public List<Long> getQuickFilterIds() {
        return quickFilterIds;
    }

    public void setQuickFilterIds(List<Long> quickFilterIds) {
        this.quickFilterIds = quickFilterIds;
    }

    public Map<String, Object> getSearchArgs() {
        return searchArgs;
    }

    public void setSearchArgs(Map<String, Object> searchArgs) {
        this.searchArgs = searchArgs;
    }

    public Map<String, Object> getAdvancedSearchArgs() {
        return advancedSearchArgs;
    }

    public void setAdvancedSearchArgs(Map<String, Object> advancedSearchArgs) {
        this.advancedSearchArgs = advancedSearchArgs;
    }

    public Map<String, Object> getOtherArgs() {
        return otherArgs;
    }

    public void setOtherArgs(Map<String, Object> otherArgs) {
        this.otherArgs = otherArgs;
    }

    public String getContent() {
        return content;
    }

    public void setContent(String content) {
        this.content = content;
    }

    public void setAssigneeFilterIds(List<Long> assigneeFilterIds) {
        this.assigneeFilterIds = assigneeFilterIds;
    }

    public List<Long> getAssigneeFilterIds() {
        return assigneeFilterIds;
    }

    public List<String> getApplyTypes() {
        return applyTypes;
    }

    public void setApplyTypes(List<String> applyTypes) {
        this.applyTypes = applyTypes;
    }

    @Override
    public String toString() {
        return StringUtil.getToString(this);
    }
}

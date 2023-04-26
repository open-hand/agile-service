package io.choerodon.agile.api.vo;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnore;
import io.swagger.annotations.ApiModelProperty;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;

import io.choerodon.agile.infra.utils.StringUtil;

import org.hzero.core.base.BaseConstants;
import org.hzero.core.util.Pair;
import org.hzero.starter.keyencrypt.core.Encrypt;

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
    @ApiModelProperty(value = "<编号搜索条件, 概要搜索条件>")
    private Pair<String, String> issueNumberAndSummaryPair;
    @ApiModelProperty(value = "内容")
    private List<String> contents;
    @ApiModelProperty(value = "[<编号搜索条件, 概要搜索条件>]")
    private List<Pair<String, String>> issueNumberAndSummaryPairs;
    @ApiModelProperty(value = "甘特图默认排序")
    private Boolean ganttDefaultOrder = false;
    @ApiModelProperty(value = "应用类型")
    private List<String> applyTypes;
    @ApiModelProperty(value = "展示的字段")
    private List<ObjectSchemeFieldVO> displayFields;
    @ApiModelProperty(value = "仅未完成的")
    private Boolean onlyInCompleted;

    /**
     * 处理content和contents搜索条件, 解决直接输入工作项前缀搜索不到数据的问题
     * @param issuePrefix   工作项前缀
     * @return  this
     */
    public SearchVO processContent(String issuePrefix) {
        if(StringUtils.isBlank(issuePrefix)) {
            return this;
        }
        // 处理content
        if(StringUtils.isNotBlank(this.content)) {
            this.issueNumberAndSummaryPair = processOneContent(issuePrefix, this.content);
            if(this.issueNumberAndSummaryPair != null) {
                this.content = null;
            }
        }
        // 处理contents
        if(CollectionUtils.isNotEmpty(this.contents)) {
            List<Pair<String, String>> issueNumberAndSummaryPairs = new ArrayList<>();
            for (String content : this.contents) {
                final Pair<String, String> oneIssueNumberAndSummaryPair = processOneContent(issuePrefix, content);
                if(oneIssueNumberAndSummaryPair != null) {
                    issueNumberAndSummaryPairs.add(oneIssueNumberAndSummaryPair);
                }
            }
            if(CollectionUtils.isNotEmpty(issueNumberAndSummaryPairs)) {
                this.issueNumberAndSummaryPairs = issueNumberAndSummaryPairs;
                this.contents = null;
            }
        }

        return this;
    }

    /**
     * 根据工作项前缀和content猜测用户想搜什么<br/>
     * 例如: 前缀为"yq-pm"<br/>
     * <ul>
     *     <li>"yq-pm-123" => {"123", "yq-pm-123"}</li>
     *     <li>"pm-123" => {"123", "pm-123"}</li>
     *     <li>"-123" => {"123", "-123"}</li>
     *     <li>"-" => {"", "-"}</li>
     *     <li>"hello" => {"hello", "hello"}</li>
     *     <li>"yq-pm" => {"", "yq-pm"}</li>
     *     <li>"-p" => {"", "-p"}</li>
     *     <li>"yq-pm-123-" => {"yq-pm-123-", "yq-pm-123-"}</li>
     *     <li>"666-123" => {"666-123", "666-123"}</li>
     * </ul>
     * @param issuePrefix   工作项前缀
     * @param content       content
     * @return              处理结果{issueNum查询条件, summary查询条件}
     */
    public static  Pair<String, String> processOneContent(String issuePrefix, String content) {
        if(StringUtils.isBlank(issuePrefix) || StringUtils.isBlank(content)) {
            return null;
        }
        // 如果工作项前缀包含了查询条件, 说明issueNum根本不参与查询, 将issueNum查询条件置空, summary查询条件原样返回
        if(issuePrefix.contains(content)) {
            return new Pair<>(StringUtils.EMPTY, content);
        }
        // 按最后一个'-'分割字符串
        final int lastMiddleLineIndex = content.lastIndexOf(BaseConstants.Symbol.MIDDLE_LINE);
        // 如果根本就没有'-', 则原样返回
        if(lastMiddleLineIndex == -1) {
            return new Pair<>(content, content);
        }
        // 取得最后一个'-'之前和之后的字符串
        final String prefixSubString = content.substring(0, lastMiddleLineIndex);
        final String suffixSubString = content.substring(lastMiddleLineIndex + 1);
        if(issuePrefix.endsWith(prefixSubString)) {
            // 如果工作项前缀包含了前序字符串, 且后序字符串不为空, 则将issueNum查询条件置为后序字符串, summary查询条件原样返回
            return new Pair<>(suffixSubString, content);
        } else {
            // 猜不出来, 原样返回
            return new Pair<>(content, content);
        }
    }

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

    /**
     * @return [&lt;编号搜索条件, 概要搜索条件&gt;]
     */
    public List<Pair<String, String>> getIssueNumberAndSummaryPairs() {
        return issueNumberAndSummaryPairs;
    }

    public SearchVO setIssueNumberAndSummaryPairs(List<Pair<String, String>> issueNumberAndSummaryPairs) {
        this.issueNumberAndSummaryPairs = issueNumberAndSummaryPairs;
        return this;
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

    /**
     * @return &lt;编号搜索条件, 概要搜索条件&gt;
     */
    public Pair<String, String> getIssueNumberAndSummaryPair() {
        return issueNumberAndSummaryPair;
    }

    public SearchVO setIssueNumberAndSummaryPair(Pair<String, String> issueNumberAndSummaryPair) {
        this.issueNumberAndSummaryPair = issueNumberAndSummaryPair;
        return this;
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

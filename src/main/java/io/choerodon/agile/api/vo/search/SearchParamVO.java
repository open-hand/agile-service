package io.choerodon.agile.api.vo.search;

import java.util.List;
import java.util.Set;

import io.swagger.annotations.ApiModelProperty;

import io.choerodon.agile.api.vo.ObjectSchemeFieldVO;

import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author superlee
 * @since 2022-11-02
 */
public class SearchParamVO {
    @ApiModelProperty("筛选条件")
    private List<Condition> conditions;
    @ApiModelProperty("高级筛选条件")
    private List<Condition> advancedConditions;
    @ApiModelProperty("树形视图")
    private Boolean treeFlag;
    @ApiModelProperty("是否统计子问题的数量")
    private Boolean countSubIssue;

    @Encrypt
    private List<Long> quickFilterIds;
    @Encrypt
    private Set<Long> issueIds;
    @ApiModelProperty("瀑布项目使用，是否包含空冲刺")
    private Boolean containsEmptySprint;
    @ApiModelProperty("展示的字段")
    private List<ObjectSchemeFieldVO> displayFields;

    public List<ObjectSchemeFieldVO> getDisplayFields() {
        return displayFields;
    }

    public void setDisplayFields(List<ObjectSchemeFieldVO> displayFields) {
        this.displayFields = displayFields;
    }

    public Boolean getContainsEmptySprint() {
        return containsEmptySprint;
    }

    public void setContainsEmptySprint(Boolean containsEmptySprint) {
        this.containsEmptySprint = containsEmptySprint;
    }

    public Set<Long> getIssueIds() {
        return issueIds;
    }

    public void setIssueIds(Set<Long> issueIds) {
        this.issueIds = issueIds;
    }

    public Boolean getCountSubIssue() {
        return countSubIssue;
    }

    public void setCountSubIssue(Boolean countSubIssue) {
        this.countSubIssue = countSubIssue;
    }

    public List<Long> getQuickFilterIds() {
        return quickFilterIds;
    }

    public void setQuickFilterIds(List<Long> quickFilterIds) {
        this.quickFilterIds = quickFilterIds;
    }

    public Boolean getTreeFlag() {
        return treeFlag;
    }

    public void setTreeFlag(Boolean treeFlag) {
        this.treeFlag = treeFlag;
    }

    public List<Condition> getConditions() {
        return conditions;
    }

    public void setConditions(List<Condition> conditions) {
        this.conditions = conditions;
    }

    public List<Condition> getAdvancedConditions() {
        return advancedConditions;
    }

    public void setAdvancedConditions(List<Condition> advancedConditions) {
        this.advancedConditions = advancedConditions;
    }

}

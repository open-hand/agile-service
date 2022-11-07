package io.choerodon.agile.api.vo.search;

import java.util.List;

import io.swagger.annotations.ApiModelProperty;

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

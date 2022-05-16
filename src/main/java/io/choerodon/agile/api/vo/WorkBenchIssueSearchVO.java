package io.choerodon.agile.api.vo;


import io.swagger.annotations.ApiModelProperty;

/**
 * @author superlee
 * @since 2020-10-22
 */
public class WorkBenchIssueSearchVO {
    @ApiModelProperty(value = "类型")
    private String type;
    @ApiModelProperty(value = "搜索参数")
    private SearchVO searchVO;

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public SearchVO getSearchVO() {
        return searchVO;
    }

    public void setSearchVO(SearchVO searchVO) {
        this.searchVO = searchVO;
    }
}

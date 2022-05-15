package io.choerodon.agile.api.vo;

import io.swagger.annotations.ApiModelProperty;
import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;
import java.util.Set;

/**
 * @author zhaotianxin
 * @date 2021-10-14 14:25
 */
public class GanttChartSearchVO {

    @Encrypt
    @ApiModelProperty(value = "问题id集合")
    private Set<Long> issueIds;
    @ApiModelProperty(value = "展示的字段")
    private List<ObjectSchemeFieldVO> displayFields;

    public Set<Long> getIssueIds() {
        return issueIds;
    }

    public void setIssueIds(Set<Long> issueIds) {
        this.issueIds = issueIds;
    }

    public List<ObjectSchemeFieldVO> getDisplayFields() {
        return displayFields;
    }

    public void setDisplayFields(List<ObjectSchemeFieldVO> displayFields) {
        this.displayFields = displayFields;
    }

}

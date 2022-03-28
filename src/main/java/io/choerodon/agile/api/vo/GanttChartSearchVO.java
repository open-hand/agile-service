package io.choerodon.agile.api.vo;

import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;
import java.util.Set;

/**
 * @author zhaotianxin
 * @date 2021-10-14 14:25
 */
public class GanttChartSearchVO {

    @Encrypt
    private Set<Long> issueIds;

    private List<ObjectSchemeFieldVO> displayFields;

    private Boolean spentWorkTimeWithSub;

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

    public Boolean getSpentWorkTimeWithSub() {
        return spentWorkTimeWithSub;
    }

    public void setSpentWorkTimeWithSub(Boolean spentWorkTimeWithSub) {
        this.spentWorkTimeWithSub = spentWorkTimeWithSub;
    }
}

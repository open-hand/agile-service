package io.choerodon.agile.api.vo;

import org.apache.commons.lang3.StringUtils;
import org.hzero.starter.keyencrypt.core.Encrypt;
import org.springframework.util.ObjectUtils;

import javax.validation.constraints.NotEmpty;
import java.util.Collections;
import java.util.List;

/**
 * @author superlee
 * @since 2020-09-02
 */
public class BurnDownSearchVO {

    @NotEmpty(message = "error.query.type.empty")
    private String type;
    @Encrypt
    private Long assigneeId;
    private Boolean onlyStory;
    @Encrypt
    private List<Long> quickFilterIds;

    private String ordinalType;

    private String filterSql;

    public String getFilterSql() {
        return filterSql;
    }

    public void setFilterSql(String filterSql) {
        this.filterSql = filterSql;
    }

    public String getOrdinalType() {
        return ordinalType;
    }

    public void setOrdinalType(String ordinalType) {
        this.ordinalType = ordinalType;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public Long getAssigneeId() {
        return assigneeId;
    }

    public void setAssigneeId(Long assigneeId) {
        this.assigneeId = assigneeId;
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

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        String delimiter = ":";
        builder.append(type);
        if (!ObjectUtils.isEmpty(assigneeId)) {
            builder.append(delimiter).append(assigneeId);
        }
        if (!ObjectUtils.isEmpty(onlyStory)) {
            builder.append(delimiter).append(onlyStory);
        }
        if (!ObjectUtils.isEmpty(quickFilterIds)) {
            Collections.sort(quickFilterIds);
            builder.append(delimiter).append(StringUtils.join(quickFilterIds, delimiter));
        }
        return builder.toString();
    }
}

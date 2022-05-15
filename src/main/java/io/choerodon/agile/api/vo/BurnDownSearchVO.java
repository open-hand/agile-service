package io.choerodon.agile.api.vo;

import com.fasterxml.jackson.annotation.JsonIgnore;
import io.swagger.annotations.ApiModelProperty;
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
    @ApiModelProperty("类型")
    private String type;
    @Encrypt
    @ApiModelProperty("经办人")
    private Long assigneeId;
    @ApiModelProperty("只查询故事")
    private Boolean onlyStory;
    @Encrypt
    @ApiModelProperty("快查id")
    private List<Long> quickFilterIds;
    @Encrypt
    @ApiModelProperty("个人筛选")
    private List<Long> personalFilterIds;
    @ApiModelProperty("原始类型")
    private String ordinalType;

    @JsonIgnore
    private String filterSql;
    @JsonIgnore
    private List<SearchVO> searchList;
    @ApiModelProperty("搜索条件")
    private SearchVO currentSearchVO;

    public SearchVO getCurrentSearchVO() {
        return currentSearchVO;
    }

    public void setCurrentSearchVO(SearchVO currentSearchVO) {
        this.currentSearchVO = currentSearchVO;
    }

    public List<SearchVO> getSearchList() {
        return searchList;
    }

    public void setSearchList(List<SearchVO> searchList) {
        this.searchList = searchList;
    }

    public List<Long> getPersonalFilterIds() {
        return personalFilterIds;
    }

    public void setPersonalFilterIds(List<Long> personalFilterIds) {
        this.personalFilterIds = personalFilterIds;
    }

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
        builder
                .append(type)
                .append(delimiter)
                .append(assigneeId)
                .append(delimiter)
                .append(onlyStory)
                .append(delimiter)
        ;
        if (!ObjectUtils.isEmpty(quickFilterIds)) {
            Collections.sort(quickFilterIds);
            builder.append(StringUtils.join(quickFilterIds, ","));
        } else {
            builder.append("null");
        }
        if (!ObjectUtils.isEmpty(personalFilterIds)) {
            Collections.sort(personalFilterIds);
            builder.append(delimiter).append(StringUtils.join(personalFilterIds, ","));
        } else {
            builder.append(delimiter).append("null");
        }
        if (!ObjectUtils.isEmpty(currentSearchVO)){
            builder.append(delimiter).append(currentSearchVO.toString().hashCode());
        }else {
            builder.append(delimiter).append("null");
        }
        return builder.toString();
    }
}

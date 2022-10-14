package io.choerodon.agile.api.vo.waterfall;

import java.util.Objects;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModelProperty;

import org.hzero.starter.keyencrypt.core.Encrypt;

/**
 * @author superlee
 * @since 2022-02-25
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class GanttParentVO {

    public static final String ISSUE = "issue";

    public static final String SPRINT = "sprint";

    @Encrypt
    @ApiModelProperty(value = "主键")
    private Long id;
    @ApiModelProperty(value = "类型")
    private String type;

    @JsonIgnore
    @ApiModelProperty(value = "父级冲刺集合")
    private Set<Long> parentSprintIds;

    public Set<Long> getParentSprintIds() {
        return parentSprintIds;
    }

    public void setParentSprintIds(Set<Long> parentSprintIds) {
        this.parentSprintIds = parentSprintIds;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof GanttParentVO)) return false;
        GanttParentVO that = (GanttParentVO) o;
        return Objects.equals(getId(), that.getId()) && Objects.equals(getType(), that.getType());
    }

    @Override
    public int hashCode() {
        return Objects.hash(getId(), getType());
    }

    @Override
    public String toString() {
        return "GanttParentVO{" +
                "id=" + id +
                ", type='" + type + '\'' +
                ", parentSprintIds=" + parentSprintIds +
                '}';
    }
}

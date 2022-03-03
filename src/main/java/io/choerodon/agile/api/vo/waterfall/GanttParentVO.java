package io.choerodon.agile.api.vo.waterfall;

import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.Objects;

/**
 * @author superlee
 * @since 2022-02-25
 */
public class GanttParentVO {

    @Encrypt
    private Long id;

    private String type;

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
}

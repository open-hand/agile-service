package io.choerodon.agile.api.vo;

import org.hzero.starter.keyencrypt.core.Encrypt;

import java.util.List;

/**
 * @author superlee
 * @since 2021-10-09
 */
public class GanttDimensionListVO {
    @Encrypt
    private List<Long> ids;

    public List<Long> getIds() {
        return ids;
    }

    public void setIds(List<Long> ids) {
        this.ids = ids;
    }
}

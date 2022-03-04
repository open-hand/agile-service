package io.choerodon.agile.api.vo.waterfall;

import org.springframework.util.ObjectUtils;

import java.util.Map;
import java.util.Set;

/**
 * @author superlee
 * @since 2022-03-03
 */
public class GanttParentInfoVO {
    private Map<Long, Set<GanttParentVO>> sonParentMap;

    private Map<Long, Set<Long>> sonSprintMap;

    public GanttParentInfoVO(Map<Long, Set<GanttParentVO>> sonParentMap, Map<Long, Set<Long>> sonSprintMap) {
        this.sonParentMap = sonParentMap;
        this.sonSprintMap = sonSprintMap;
    }

    public Map<Long, Set<GanttParentVO>> getSonParentMap() {
        return sonParentMap;
    }

    public void setSonParentMap(Map<Long, Set<GanttParentVO>> sonParentMap) {
        this.sonParentMap = sonParentMap;
    }

    public Map<Long, Set<Long>> getSonSprintMap() {
        return sonSprintMap;
    }

    public void setSonSprintMap(Map<Long, Set<Long>> sonSprintMap) {
        this.sonSprintMap = sonSprintMap;
    }

    public boolean isEmpty() {
        return ObjectUtils.isEmpty(sonParentMap)
                && ObjectUtils.isEmpty(sonSprintMap);
    }
}

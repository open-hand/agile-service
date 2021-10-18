package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.infra.dto.WorkCalendarSubscribeDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Set;

/**
 * @author huaxin.deng@hand-china.com 2021-10-11 14:42:49
 */
public interface WorkCalendarSubscribeMapper extends BaseMapper<WorkCalendarSubscribeDTO> {

    List<WorkCalendarSubscribeDTO> queryByUserIds(@Param("organizationId") Long organizationId,
                                                  @Param("userIds") Set<Long> userIds);
}

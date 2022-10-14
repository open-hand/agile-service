package io.choerodon.agile.infra.mapper;

import java.util.Date;
import java.util.List;

import org.apache.ibatis.annotations.Param;

import io.choerodon.agile.infra.dto.WorkCalendarRefDTO;
import io.choerodon.mybatis.common.BaseMapper;

/**
 * @author dinghuang123@gmail.com
 * @since 2018/10/10
 */
public interface WorkCalendarRefMapper extends BaseMapper<WorkCalendarRefDTO> {

    /**
     * 根据冲刺id查询冲刺加班日期
     *
     * @param sprintId  sprintId
     * @param projectId projectId
     * @return Date
     */
    List<Date> queryWorkBySprintIdAndProjectId(@Param("sprintId") Long sprintId, @Param("projectId") Long projectId);

    /**
     * 根据冲刺id查询冲刺节假日期
     *
     * @param sprintId  sprintId
     * @param projectId projectId
     * @return Date
     */
    List<Date> queryHolidayBySprintIdAndProjectId(@Param("sprintId") Long sprintId, @Param("projectId") Long projectId);

    /**
     * 查询今年包括下一年的数据
     *
     * @param projectId projectId
     * @param sprintId  sprintId
     * @param year      year
     * @return SprintWorkCalendarRefDO
     */
    List<WorkCalendarRefDTO> queryWithNextYearByYear(@Param("projectId")Long projectId, @Param("sprintId")Long sprintId, @Param("year")Integer year);

    /**
     * 根据项目id查工作日历设置
     *
     * @param projectId projectId
     * @param year year
     * @return result
     */
    List<WorkCalendarRefDTO> listByProjectId(@Param("projectId") Long projectId, @Param("year") Integer year);

    List<WorkCalendarRefDTO> selectEffectiveHolidays(@Param("projectId") Long projectId,
                                                     @Param("startDate") Date startDate,
                                                     @Param("endDate") Date endDate);

}

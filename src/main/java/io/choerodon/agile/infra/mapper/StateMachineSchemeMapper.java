package io.choerodon.agile.infra.mapper;

import java.util.List;

import org.apache.ibatis.annotations.Param;
import org.springframework.stereotype.Component;

import io.choerodon.agile.infra.dto.StateMachineSchemeDTO;
import io.choerodon.mybatis.common.BaseMapper;

/**
 * @author peng.jiang@hand-china.com
 */
@Component
public interface StateMachineSchemeMapper extends BaseMapper<StateMachineSchemeDTO> {

    /**
     * 分页查询状态方案
     *
     * @param scheme 状态机方案
     * @param param  模糊查询参数
     * @return 方案列表
     */
    List<StateMachineSchemeDTO> fulltextSearch(@Param("scheme") StateMachineSchemeDTO scheme, @Param("param") String param);

    /**
     * 根据id列表查询，附带配置
     *
     * @return 方案列表
     */
    List<StateMachineSchemeDTO> queryByIdsWithConfig(@Param("organizationId") Long organizationId, @Param("schemeIds") List<Long> schemeIds);

    /**
     * 根据id列表查询
     *
     * @return 方案列表
     */
    List<StateMachineSchemeDTO> queryByIds(@Param("organizationId") Long organizationId, @Param("schemeIds") List<Long> schemeIds);

    /**
     * 查询出组织下所有方案
     *
     * @param organizationId organizationId
     * @return result
     */
    List<StateMachineSchemeDTO> queryByOrgId(@Param("organizationId") Long organizationId);

    /**
     * 更新发布进度
     *
     * @param organizationId organizationId
     * @param schemeId       schemeId
     * @param deployProgress deployProgress
     * @return Integer
     */
    Integer updateDeployProgress(@Param("organizationId") Long organizationId, @Param("schemeId") Long schemeId, @Param("deployProgress") Integer deployProgress);

    /**
     * 更新发布状态
     *
     * @param organizationId organizationId
     * @param schemeId schemeId
     * @param deployStatus deployStatus
     */
    void updateDeployStatus(@Param("organizationId") Long organizationId, @Param("schemeId") Long schemeId, @Param("deployStatus") String deployStatus);
}

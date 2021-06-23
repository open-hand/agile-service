package io.choerodon.agile.app.service;

import java.util.List;

import io.choerodon.agile.api.vo.CustomChartCreateVO;
import io.choerodon.agile.api.vo.CustomChartUpdateVO;
import io.choerodon.agile.api.vo.CustomChartVO;

/**
 * @author chihao.ran@hand-china.com
 * 2021/06/21 16:33
 */
public interface CustomChartService {
    /**
     * 查询项目下的自定义报表
     * @param projectId 项目id
     * @return 项目下的自定义报表
     */
    List<CustomChartVO> queryListByProject(Long projectId);

    /**
     * 创建自定义报表
     * @param projectId 项目id
     * @param customChartCreate 要创建的自定义报表
     * @return 要创建的自定义报表
     */
    CustomChartVO createCustomChart(Long projectId, CustomChartCreateVO customChartCreate);

    /**
     * 更新自定义报表
     * @param projectId 项目id
     * @param customChartId 要更新的自定义报表id
     * @param customChartUpdate 更新的内容
     * @return 更新后的自定义报表
     */
    CustomChartVO updateCustomChart(Long projectId, Long customChartId, CustomChartUpdateVO customChartUpdate);

    /**
     * 查询自定义报表详情
     * @param projectId 项目id
     * @param customChartId 自定义报表id
     * @return 自定义报表详情
     */
    CustomChartVO queryCustomChartDetail(Long projectId, Long customChartId);

    /**
     * 校验自定义报表名称是否重复
     * @param projectId 项目id
     * @param name 名称
     * @return 校验自定义报表名称是否重复
     */
    Boolean checkName(Long projectId, String name);
}

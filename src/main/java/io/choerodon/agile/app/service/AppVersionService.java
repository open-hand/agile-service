package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.AppVersionVO;

/**
 * @author superlee
 * @since 2021-03-10
 */
public interface AppVersionService {

    /**
     * 创建应用版本
     * @param projectId 项目id
     * @param appVersionVO 要创建的应用版本
     * @return 创建的应用版本
     */
    AppVersionVO createAppVersion(Long projectId, AppVersionVO appVersionVO);

    /**
     * 更新应用版本
     * @param projectId 项目id
     * @param appVersionId 要更新的应用版本id
     * @param appVersionVO 更新的应用版本内容
     * @return 更新的应用版本内容
     */
    AppVersionVO updateAppVersion(Long projectId, Long appVersionId, AppVersionVO appVersionVO);


    /**
     * 根据id查询单条应用版本
     * @param projectId 项目id
     * @param appVersionId 应用版本id
     * @return 应用版本
     */
    AppVersionVO queryAppVersionById(Long projectId, Long appVersionId);

    /**
     * 删除应用版本
     * @param projectId 项目id
     * @param appVersionId 应用版本id
     */
    void deleteAppVersion(Long projectId, Long appVersionId);

    /**
     * 是否重复
     * @param appVersionVO 要校验的应用版本tag
     * @return 是否重复
     */
    Boolean checkTagRepeat(AppVersionVO appVersionVO);
}

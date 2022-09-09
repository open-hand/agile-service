package io.choerodon.agile.app.service;

import java.util.List;

import io.choerodon.agile.api.vo.PersonalFilterVO;

/**
 * @author shinan.chen
 * @since 2019/2/25
 */
public interface PersonalFilterService {

    PersonalFilterVO queryById(Long organizationId, Long projectId, Long filterId);

    PersonalFilterVO create(Long organizationId, Long projectId, PersonalFilterVO personalFilterVO);

    PersonalFilterVO update(Long organizationId, Long projectId, Long filterId, PersonalFilterVO personalFilterVO);

    void deleteById(Long organizationId, Long projectId, Long filterId);

    List<PersonalFilterVO> listByUserId(Long organizationId, Long projectId, Long userId, String searchStr, String filterTypeCode);

    /**
     * 根据唯一性约束查询数据库中是否存在重复的记录
     * @param organizationId 组织ID
     * @param projectId 项目ID
     * @param userId 用户ID
     * @param name 个人筛选名称
     * @param filterTypeCode 个人筛选类型
     * @param filterId 个人筛选ID(用于更新检查时排除自身)
     * @return 是否存在重复的记录
     */
    boolean nameIsExist(Long organizationId, Long projectId, Long userId, String name, String filterTypeCode, Long filterId);

    Boolean setDefault(Long organizationId, Long projectId, Long filterId);
}

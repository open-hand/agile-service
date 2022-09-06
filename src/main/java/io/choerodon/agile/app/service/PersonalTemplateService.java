package io.choerodon.agile.app.service;

import java.util.List;

import io.choerodon.agile.api.vo.PersonalTemplateCreateVO;
import io.choerodon.agile.api.vo.PersonalTemplateUpdateVO;
import io.choerodon.agile.api.vo.PersonalTemplatelVO;

/**
 * @author huaxin.deng@hand-china.com 2021-02-02 13:56:07
 */
public interface PersonalTemplateService {

    /**
     * 创建模板
     *
     * @param projectId projectId
     * @param personalTemplateCreateVO personalTemplateCreateVO
     * @return result
     */
    PersonalTemplatelVO create(Long projectId, PersonalTemplateCreateVO personalTemplateCreateVO);

    /**
     * 修改模板
     *
     * @param projectId projectId
     * @param id id
     * @param personalTemplateUpdateVO personalTemplateUpdateVO
     * @return result
     */
    PersonalTemplatelVO update(Long projectId, Long id, PersonalTemplateUpdateVO personalTemplateUpdateVO);

    /**
     * 删除模板
     *
     * @param projectId projectId
     * @param id id
     */
    void delete(Long projectId, Long id);

    /**
     * 根据用户和类型查询模板
     *
     * @param projectId projectId
     * @param userId userId
     * @param action action
     * @param type type
     * @return result
     */
    List<PersonalTemplatelVO> queryByUserAndAction(Long projectId, Long userId, String action, String type);

    /**
     * 根据用户和类型进行名称校验
     *
     * @param projectId projectId
     * @param userId userId
     * @param name name
     * @param action action
     * @param type type
     * @return result
     */
    Boolean checkNameByAction(Long projectId, Long userId, String name, String action, String type);
}

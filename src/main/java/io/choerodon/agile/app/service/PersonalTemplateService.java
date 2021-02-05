package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.PersonalTemplateCreateVO;
import io.choerodon.agile.api.vo.PersonalTemplatelVO;
import io.choerodon.agile.api.vo.PersonalTemplateUpdateVO;

import java.util.List;

/**
 * @author huaxin.deng@hand-china.com 2021-02-02 13:56:07
 */
public interface PersonalTemplateService {

    /**
     * 创建模板
     *
     * @param projectId
     * @param personalTemplateCreateVO
     * @return
     */
    PersonalTemplatelVO create(Long projectId, PersonalTemplateCreateVO personalTemplateCreateVO);

    /**
     * 修改模板
     *
     * @param projectId
     * @param id
     * @param personalTemplateUpdateVO
     * @return
     */
    PersonalTemplatelVO update(Long projectId, Long id, PersonalTemplateUpdateVO personalTemplateUpdateVO);

    /**
     * 删除模板
     *
     * @param projectId
     * @param id
     */
    void delete(Long projectId, Long id);

    /**
     * 根据用户和类型查询模板
     *
     * @param projectId
     * @param userId
     * @param action
     * @param type
     * @return
     */
    List<PersonalTemplatelVO> queryByUserAndAction(Long projectId, Long userId, String action, String type);

    /**
     * 根据用户和类型进行名称校验
     *
     * @param projectId
     * @param userId
     * @param name
     * @param action
     * @param type
     * @return
     */
    Boolean checkNameByAction(Long projectId, Long userId, String name, String action, String type);
}

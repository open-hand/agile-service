package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.PersonalFilterVO;

import java.util.List;

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

    Boolean checkName(Long organizationId, Long projectId, Long userId, String name, String filterTypeCode);

    Boolean setDefault(Long organizationId, Long projectId, Long filterId);
}

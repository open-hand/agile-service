package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.ListLayoutVO;

/**
 * @author superlee
 * @since 2021-10-19
 */
public interface OrganizationListLayoutService {

    ListLayoutVO queryByApplyType(Long organizationId, String applyType);

    ListLayoutVO save(Long organizationId, ListLayoutVO listLayoutVO);
}

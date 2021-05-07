package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.ListLayoutVO;

/**
 * @author zhaotianxin
 * @date 2021-05-07 14:11
 */
public interface ListLayoutService {
   ListLayoutVO save(Long organizationId, Long projectId, ListLayoutVO listLayoutVO);

   ListLayoutVO queryByTypeCode(Long organizationId, Long projectId, String typeCode);
}

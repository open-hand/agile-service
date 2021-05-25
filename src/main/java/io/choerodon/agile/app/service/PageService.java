package io.choerodon.agile.app.service;

import io.choerodon.core.domain.Page;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.agile.api.vo.PageSearchVO;
import io.choerodon.agile.api.vo.PageVO;

/**
 * @author shinan.chen
 * @since 2019/4/1
 */
public interface PageService {

    Page<PageVO> pageQuery(Long organizationId, PageRequest pageRequest, PageSearchVO searchDTO);

}

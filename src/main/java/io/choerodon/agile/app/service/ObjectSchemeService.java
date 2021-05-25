package io.choerodon.agile.app.service;

import io.choerodon.core.domain.Page;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import io.choerodon.agile.api.vo.ObjectSchemeSearchVO;
import io.choerodon.agile.api.vo.ObjectSchemeVO;

/**
 * @author shinan.chen
 * @since 2019/3/29
 */
public interface ObjectSchemeService {

    Page<ObjectSchemeVO> pageQuery(Long organizationId, PageRequest pageRequest, ObjectSchemeSearchVO searchDTO);
}

package io.choerodon.agile.app.service;

import com.github.pagehelper.PageInfo;
import org.springframework.data.domain.Pageable;
import io.choerodon.agile.api.vo.PageSearchVO;
import io.choerodon.agile.api.vo.PageVO;

/**
 * @author shinan.chen
 * @since 2019/4/1
 */
public interface PageService {
    PageInfo<PageVO> pageQuery(Long organizationId, Pageable pageable, PageSearchVO searchDTO);
}

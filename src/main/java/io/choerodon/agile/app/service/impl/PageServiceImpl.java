package io.choerodon.agile.app.service.impl;

import com.github.pagehelper.PageHelper;
import com.github.pagehelper.PageInfo;
import io.choerodon.web.util.PageableHelper;
import org.springframework.data.domain.Pageable;
import io.choerodon.agile.api.vo.PageSearchVO;
import io.choerodon.agile.api.vo.PageVO;
import io.choerodon.agile.app.service.PageService;
import io.choerodon.agile.infra.dto.PageDTO;
import io.choerodon.agile.infra.mapper.PageMapper;
import io.choerodon.agile.infra.utils.PageUtil;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * @author shinan.chen
 * @since 2019/4/1
 */
@Service
public class PageServiceImpl implements PageService {
    @Autowired
    private PageMapper pageMapper;
    @Autowired
    private ModelMapper modelMapper;

    @Override
    public PageInfo<PageVO> pageQuery(Long organizationId, Pageable pageable, PageSearchVO searchDTO) {
        PageInfo<PageDTO> page = PageHelper.startPage(pageable.getPageNumber(), pageable.getPageSize(),
                PageableHelper.getSortSql(pageable.getSort())).doSelectPageInfo(() -> pageMapper.fulltextSearch(organizationId, searchDTO));
        return PageUtil.buildPageInfoWithPageInfoList(page,
                modelMapper.map(page.getList(), new TypeToken<List<PageVO>>() {
                }.getType()));
    }
}

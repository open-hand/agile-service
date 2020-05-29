package io.choerodon.agile.app.service.impl;

import io.choerodon.core.domain.Page;
import io.choerodon.mybatis.pagehelper.PageHelper;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
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
    public Page<PageVO> pageQuery(Long organizationId, PageRequest pageRequest, PageSearchVO searchDTO) {
        Page<PageDTO> page = PageHelper.doPageAndSort(pageRequest, () -> pageMapper.fulltextSearch(organizationId, searchDTO));
        return PageUtil.buildPageInfoWithPageInfoList(page,
                modelMapper.map(page.getContent(), new TypeToken<List<PageVO>>() {
                }.getType()));
    }
}

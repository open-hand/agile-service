package io.choerodon.agile.app.service.impl;

import com.github.pagehelper.PageHelper;
import com.github.pagehelper.PageInfo;
import io.choerodon.web.util.PageableHelper;
import org.springframework.data.domain.Pageable;
import io.choerodon.agile.api.vo.ObjectSchemeSearchVO;
import io.choerodon.agile.api.vo.ObjectSchemeVO;
import io.choerodon.agile.app.service.ObjectSchemeService;
import io.choerodon.agile.infra.dto.ObjectSchemeDTO;
import io.choerodon.agile.infra.mapper.ObjectSchemeMapper;
import io.choerodon.agile.infra.utils.PageUtil;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * @author shinan.chen
 * @since 2019/3/29
 */
@Service
public class ObjectSchemeServiceImpl implements ObjectSchemeService {
    @Autowired
    private ObjectSchemeMapper objectSchemeMapper;
    @Autowired
    private ModelMapper modelMapper;
    @Override
    public PageInfo<ObjectSchemeVO> pageQuery(Long organizationId, Pageable pageable, ObjectSchemeSearchVO searchDTO) {
        PageInfo<ObjectSchemeDTO> page = PageHelper.startPage(pageable.getPageNumber(), pageable.getPageSize(),
                PageableHelper.getSortSql(pageable.getSort())).doSelectPageInfo(() -> objectSchemeMapper.fulltextSearch(organizationId, searchDTO));
        return PageUtil.buildPageInfoWithPageInfoList(page,
                modelMapper.map(page.getList(), new TypeToken<List<ObjectSchemeVO>>() {
                }.getType()));
    }
}

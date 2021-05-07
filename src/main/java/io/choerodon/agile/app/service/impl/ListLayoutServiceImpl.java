package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.ListLayoutColumnRelVO;
import io.choerodon.agile.api.vo.ListLayoutVO;
import io.choerodon.agile.app.service.ListLayoutService;
import io.choerodon.agile.infra.dto.ListLayoutColumnRelDTO;
import io.choerodon.agile.infra.dto.ListLayoutDTO;
import io.choerodon.agile.infra.mapper.ListLayoutColumnRelMapper;
import io.choerodon.agile.infra.mapper.ListLayoutMapper;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.DetailsHelper;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.util.List;

/**
 * @author zhaotianxin
 * @date 2021-05-07 14:20
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class ListLayoutServiceImpl implements ListLayoutService {
    @Autowired
    private ListLayoutMapper listLayoutMapper;
    @Autowired
    private ListLayoutColumnRelMapper listLayoutColumnRelMapper;
    @Autowired
    private ModelMapper modelMapper;

    @Override
    public ListLayoutVO save(Long organizationId, Long projectId, ListLayoutVO listLayoutVO) {
        if (ObjectUtils.isEmpty(listLayoutVO.getTypeCode())) {
            throw new CommonException("error.list.layout.type.code.null");
        }
        Long userId = DetailsHelper.getUserDetails().getUserId();
        ListLayoutDTO layoutDTO = new ListLayoutDTO(listLayoutVO.getTypeCode(), userId, projectId, organizationId);
        List<ListLayoutDTO> layoutDTOS = listLayoutMapper.select(layoutDTO);
        if (CollectionUtils.isEmpty(layoutDTOS)) {
            baseInsert(layoutDTO);
        } else {
            layoutDTO = layoutDTOS.get(0);
        }
        saveColumnRel(organizationId, projectId, layoutDTO.getId(), listLayoutVO.getListLayoutColumnRelVOS());
        return queryByTypeCode(organizationId, projectId, listLayoutVO.getTypeCode());
    }

    private void saveColumnRel(Long organizationId, Long projectId, Long layoutId, List<ListLayoutColumnRelVO> listLayoutColumnRelVOS) {
        ListLayoutColumnRelDTO listLayoutColumnRelDTO = new ListLayoutColumnRelDTO();
        listLayoutColumnRelDTO.setProjectId(projectId);
        listLayoutColumnRelDTO.setOrganizationId(organizationId);
        listLayoutColumnRelDTO.setLayoutId(layoutId);
        List<ListLayoutColumnRelDTO> layoutColumnRelDTOS = listLayoutColumnRelMapper.select(listLayoutColumnRelDTO);
        if (!CollectionUtils.isEmpty(layoutColumnRelDTOS)) {
            listLayoutColumnRelMapper.delete(listLayoutColumnRelDTO);
        }
        listLayoutColumnRelVOS.forEach(v -> {
            ListLayoutColumnRelDTO layoutColumnRelDTO = modelMapper.map(v, ListLayoutColumnRelDTO.class);
            layoutColumnRelDTO.setOrganizationId(organizationId);
            layoutColumnRelDTO.setLayoutId(layoutId);
            layoutColumnRelDTO.setProjectId(projectId);
            if (listLayoutColumnRelMapper.insertSelective(layoutColumnRelDTO) != 1) {
                throw new CommonException("error.list.layout.column.rel.insert");
            }
        });
    }

    private ListLayoutDTO baseInsert(ListLayoutDTO layoutDTO) {
        if (listLayoutMapper.insertSelective(layoutDTO) != 1) {
            throw new CommonException("error.list.layout.insert");
        }
        return listLayoutMapper.selectByPrimaryKey(layoutDTO.getId());
    }

    @Override
    public ListLayoutVO queryByTypeCode(Long organizationId, Long projectId, String typeCode) {
        Long userId = DetailsHelper.getUserDetails().getUserId();
        ListLayoutDTO listLayoutDTO = new ListLayoutDTO(typeCode, userId, projectId, organizationId);
        List<ListLayoutDTO> listLayoutDTOS = listLayoutMapper.select(listLayoutDTO);
        if (CollectionUtils.isEmpty(listLayoutDTOS)) {
            return null;
        }
        ListLayoutDTO layoutDTO = listLayoutDTOS.get(0);
        ListLayoutVO layoutVO = modelMapper.map(layoutDTO, ListLayoutVO.class);
        ListLayoutColumnRelDTO listLayoutColumnRelDTO = new ListLayoutColumnRelDTO(layoutVO.getId(), projectId, organizationId);
        List<ListLayoutColumnRelDTO> list = listLayoutColumnRelMapper.select(listLayoutColumnRelDTO);
        if (!CollectionUtils.isEmpty(list)) {
            layoutVO.setListLayoutColumnRelVOS(modelMapper.map(list, new TypeToken<List<ListLayoutColumnRelVO>>() {
            }.getType()));
        }
        return layoutVO;
    }
}

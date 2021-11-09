package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.AgileUserVO;
import io.choerodon.agile.api.vo.UserVO;
import io.choerodon.agile.api.vo.WorkGroupUserRelParamVO;
import io.choerodon.agile.api.vo.WorkGroupUserRelVO;
import io.choerodon.agile.app.service.WorkGroupService;
import io.choerodon.agile.app.service.WorkGroupUserRelService;
import io.choerodon.agile.infra.dto.UserDTO;
import io.choerodon.agile.infra.dto.WorkGroupUserRelDTO;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.mapper.WorkGroupUserRelMapper;
import io.choerodon.agile.infra.utils.PageUtil;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.util.*;
import java.util.stream.Collectors;

/**
 * @author zhaotianxin
 * @date 2021-11-08 20:51
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class WorkGroupUserRelServiceImpl implements WorkGroupUserRelService {

    @Autowired
    private WorkGroupUserRelMapper workGroupUserRelMapper;

    @Autowired
    private BaseFeignClient baseFeignClient;

    @Autowired
    private WorkGroupService workGroupService;

    @Autowired
    private ModelMapper modelMapper;

    @Override
    public void batchInsertRel(Long organizationId, WorkGroupUserRelParamVO workGroupUserRelParamVO) {
        List<Long> userIds = workGroupUserRelParamVO.getUserIds();
        if (!CollectionUtils.isEmpty(userIds)) {
            Set<Long> existUserIds = workGroupUserRelMapper.queryByWorkGroupId(organizationId, workGroupUserRelParamVO.getWorkGroupId());
            userIds.stream()
                    .filter(v -> !existUserIds.contains(v))
                    .forEach(v -> {
                        WorkGroupUserRelDTO workGroupUserRelDTO = new WorkGroupUserRelDTO();
                        workGroupUserRelDTO.setWorkGroupId(workGroupUserRelParamVO.getWorkGroupId());
                        workGroupUserRelDTO.setUserId(v);
                        workGroupUserRelDTO.setOrganizationId(organizationId);
                        baseInsert(workGroupUserRelDTO);
                    });
        }
    }

    private void baseInsert(WorkGroupUserRelDTO workGroupUserRelDTO) {
        if (workGroupUserRelMapper.insertSelective(workGroupUserRelDTO) != 1) {
            throw new CommonException("error.work.group.user.rel.insert");
        }
    }

    @Override
    public void batchDeleteRel(Long organizationId, WorkGroupUserRelParamVO workGroupUserRelParamVO) {
        List<Long> userIds = workGroupUserRelParamVO.getUserIds();
        if (!CollectionUtils.isEmpty(userIds)) {
            List<Long> workGroupIds = workGroupService.listChildrenWorkGroup(organizationId, workGroupUserRelParamVO.getWorkGroupId());
            workGroupUserRelMapper.batchDelete(organizationId, workGroupIds, workGroupUserRelParamVO.getUserIds());
        }
    }

    @Override
    public Page<WorkGroupUserRelVO> pageByQuery(Long organizationId, PageRequest pageRequest, WorkGroupUserRelParamVO workGroupUserRelParamVO) {
        // 查询工作组的所有子级
        List<Long> workGroupIds = workGroupService.listChildrenWorkGroup(organizationId, workGroupUserRelParamVO.getWorkGroupId());
        Set<Long> userIds = workGroupUserRelMapper.listUserIdsByWorkGroupIds(organizationId, workGroupIds);
        if (CollectionUtils.isEmpty(userIds)) {
            return new Page<>();
        }
        AgileUserVO agileUserVO = modelMapper.map(workGroupUserRelParamVO, AgileUserVO.class);
        agileUserVO.setUserIds(userIds);
        Page<UserDTO> userPage = baseFeignClient.pagingUsersOnOrganizationLevel(organizationId, pageRequest.getPage(), pageRequest.getSize(), agileUserVO).getBody();
        List<UserDTO> content = userPage.getContent();
        if (CollectionUtils.isEmpty(content)) {
            return new Page<>();
        }
        List<WorkGroupUserRelVO> list = new ArrayList<>();
        content.forEach(v -> {
            WorkGroupUserRelVO workGroupUserRelVO = new WorkGroupUserRelVO();
            workGroupUserRelVO.setUserId(v.getId());
            workGroupUserRelVO.setUserVO(modelMapper.map(v, UserVO.class));
            list.add(workGroupUserRelVO);
        });
        return PageUtil.buildPageInfoWithPageInfoList(userPage, list);
    }

    @Override
    public Page<WorkGroupUserRelVO> pageUnAssignee(Long organizationId, PageRequest pageRequest, WorkGroupUserRelParamVO workGroupUserRelParamVO) {
        AgileUserVO agileUserVO = modelMapper.map(workGroupUserRelParamVO, AgileUserVO.class);
        // 查询已分配工作的用户
        Set<Long> existUserIds = workGroupUserRelMapper.queryByWorkGroupId(organizationId, null);
        if (ObjectUtils.isEmpty(agileUserVO)) {
            agileUserVO = new AgileUserVO();
        }
        agileUserVO.setIgnoredUserIds(existUserIds);
        Page<UserDTO> userPage = baseFeignClient.pagingUsersOnOrganizationLevel(organizationId, pageRequest.getPage(), pageRequest.getSize(), agileUserVO).getBody();
        List<UserDTO> content = userPage.getContent();
        if (CollectionUtils.isEmpty(content)) {
            return new Page<>();
        }
        List<WorkGroupUserRelVO> list = new ArrayList<>();
        content.forEach(v -> {
            WorkGroupUserRelVO workGroupUserRelVO = new WorkGroupUserRelVO();
            workGroupUserRelVO.setUserId(v.getId());
            workGroupUserRelVO.setUserVO(modelMapper.map(v, UserVO.class));
            list.add(workGroupUserRelVO);
        });
        return PageUtil.buildPageInfoWithPageInfoList(userPage, list);
    }

    @Override
    public Map<Long, Set<Long>> getWorkGroupMap(Long organizationId) {
        WorkGroupUserRelDTO workGroupUserRelDTO = new WorkGroupUserRelDTO();
        workGroupUserRelDTO.setOrganizationId(organizationId);
        List<WorkGroupUserRelDTO> workGroupUserRelDTOS = workGroupUserRelMapper.select(workGroupUserRelDTO);
        Map<Long, Set<Long>> map = new HashMap<>();
        if (!CollectionUtils.isEmpty(workGroupUserRelDTOS)) {
            map.putAll(workGroupUserRelDTOS.stream().collect(Collectors.groupingBy(WorkGroupUserRelDTO::getWorkGroupId, Collectors.mapping(WorkGroupUserRelDTO::getUserId, Collectors.toSet()))));
        }
        return map;
    }
}

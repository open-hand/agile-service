package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.service.WorkGroupService;
import io.choerodon.agile.app.service.WorkGroupUserRelService;
import io.choerodon.agile.infra.dto.UserDTO;
import io.choerodon.agile.infra.dto.WorkGroupUserRelDTO;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.mapper.WorkGroupMapper;
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

    @Autowired
    private WorkGroupMapper workGroupMapper;

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
            List<Long> workGroupIds = workGroupService.listChildrenWorkGroup(organizationId, Collections.singletonList(workGroupUserRelParamVO.getWorkGroupId()));
            workGroupUserRelMapper.batchDelete(organizationId, workGroupIds, workGroupUserRelParamVO.getUserIds());
        }
    }

    @Override
    public Page<WorkGroupUserRelVO> pageByQuery(Long organizationId, PageRequest pageRequest, WorkGroupUserRelParamVO workGroupUserRelParamVO) {
        if (ObjectUtils.isEmpty(workGroupUserRelParamVO.getWorkGroupId())) {
            throw new CommonException("error.work.group.id.null");
        }
        List<Long> workGroupIds = Collections.singletonList(workGroupUserRelParamVO.getWorkGroupId());
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
        List<Long> users = content.stream().map(UserDTO::getId).collect(Collectors.toList());
        List<WorkGroupVO> workGroupVOS = workGroupUserRelMapper.selectWorkGroupByUserId(organizationId, users);
        Map<Long, List<WorkGroupVO>> workGroupMap = new HashMap<>();
        if (!CollectionUtils.isEmpty(workGroupVOS)) {
            workGroupMap.putAll(workGroupVOS.stream().collect(Collectors.groupingBy(WorkGroupVO::getUserId)));
        }
        List<WorkGroupUserRelVO> list = new ArrayList<>();
        content.forEach(v -> {
            WorkGroupUserRelVO workGroupUserRelVO = new WorkGroupUserRelVO();
            workGroupUserRelVO.setUserId(v.getId());
            workGroupUserRelVO.setUserVO(modelMapper.map(v, UserVO.class));
            workGroupUserRelVO.setWorkGroupVOS(workGroupMap.get(v.getId()));
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

    @Override
    public Page<WorkGroupUserRelVO> pageUnlinkUser(Long organizationId, PageRequest pageRequest, WorkGroupUserRelParamVO workGroupUserRelParamVO) {
        AgileUserVO agileUserVO = modelMapper.map(workGroupUserRelParamVO, AgileUserVO.class);
        if (!ObjectUtils.isEmpty(workGroupUserRelParamVO.getWorkGroupId())) {
            // 传了工作组id就要忽略工作组已关联的成员
            Set<Long> userIds = workGroupUserRelMapper.listUserIdsByWorkGroupIds(organizationId, Arrays.asList(workGroupUserRelParamVO.getWorkGroupId()));
            agileUserVO.setIgnoredUserIds(userIds);
        }
        Page<UserDTO> userPage = baseFeignClient.pagingUsersOnOrganizationLevel(organizationId, pageRequest.getPage(), pageRequest.getSize(), agileUserVO).getBody();
        List<UserDTO> content = userPage.getContent();
        if (CollectionUtils.isEmpty(content)) {
            return new Page<>();
        }
        List<Long> userIds = userPage.stream().map(UserDTO::getId).collect(Collectors.toList());
        List<WorkGroupVO> workGroupVOS = workGroupUserRelMapper.selectWorkGroupByUserId(organizationId, userIds);
        Map<Long, List<WorkGroupVO>> workGroupMap = new HashMap<>();
        if (!CollectionUtils.isEmpty(workGroupVOS)) {
            workGroupMap.putAll(workGroupVOS.stream().collect(Collectors.groupingBy(WorkGroupVO::getUserId)));
        }
        List<WorkGroupUserRelVO> list = new ArrayList<>();
        content.forEach(v -> {
            WorkGroupUserRelVO workGroupUserRelVO = new WorkGroupUserRelVO();
            workGroupUserRelVO.setUserId(v.getId());
            workGroupUserRelVO.setUserVO(modelMapper.map(v, UserVO.class));
            workGroupUserRelVO.setWorkGroupVOS(workGroupMap.get(v.getId()));
            list.add(workGroupUserRelVO);
        });
        return PageUtil.buildPageInfoWithPageInfoList(userPage, list);
    }

    @Override
    public Page<UserDTO> pageByGroups(Long organizationId, PageRequest pageRequest, WorkGroupUserRelParamVO workGroupUserRelParamVO) {
        List<Long> selectedUserIds = workGroupUserRelParamVO.getUserIds();
        List<Long> selectedWorkGroupIds = workGroupUserRelParamVO.getWorkGroupIds();
        Page<UserDTO> userPage = new Page<>();
        AgileUserVO agileUserVO = modelMapper.map(workGroupUserRelParamVO, AgileUserVO.class);
        agileUserVO.setUserIds(null);
        Set<Long> ignoredUserIds = new HashSet<>();
        // 处理分组筛选
        Boolean doPage = handlerWorkGroupIds(organizationId, selectedWorkGroupIds, ignoredUserIds, agileUserVO);
        // 过滤选中的用户
        ignoredUserIds.addAll(selectedUserIds);
        agileUserVO.setIgnoredUserIds(ignoredUserIds);
        if (Boolean.TRUE.equals(doPage)) {
            userPage = baseFeignClient.pagingUsersOnOrganizationLevel(organizationId, pageRequest.getPage(), pageRequest.getSize(), agileUserVO).getBody();
        }
        // 处理选中的用户
        appendSelectedUsers(userPage, selectedUserIds, pageRequest);
        return userPage;
    }

    private Boolean handlerWorkGroupIds(Long organizationId, List<Long> selectedWorkGroupIds, Set<Long> ignoredUserIds, AgileUserVO agileUserVO) {
        boolean doPage = true;
        if (!CollectionUtils.isEmpty(selectedWorkGroupIds)) {
            // 查询选中的工作组及子级
            List<Long> workGroupIds = workGroupService.listChildrenWorkGroup(organizationId, selectedWorkGroupIds);
            Set<Long> workGroupUserIds = new HashSet<>();
            if (!CollectionUtils.isEmpty(workGroupIds)) {
                workGroupUserIds = workGroupUserRelMapper.listUserIdsByWorkGroupIds(organizationId, workGroupIds);
            }
            // 是否包含未分配工作组
            boolean containsNoGroup = selectedWorkGroupIds.contains(0L);
            if (containsNoGroup) {
                // 包含未分配，分页查询时忽略其他未选中工作组的用户但保留已选中工作组的用户
                List<Long> unSelectedWorkGroupIds = workGroupMapper.selectIdsByOrganizationId(organizationId, workGroupIds);
                ignoredUserIds.addAll(workGroupUserRelMapper.listUserIdsByWorkGroupIds(organizationId, unSelectedWorkGroupIds));
                ignoredUserIds.removeIf(workGroupUserIds::contains);
                agileUserVO.setUserIds(null);
            } else {
                // 不包含未分配，分页查询选中工作组的用户
                agileUserVO.setUserIds(workGroupUserIds);
                if (CollectionUtils.isEmpty(workGroupUserIds)) {
                    // 选中工作组无用户时，不分页查询用户
                    doPage = false;
                }
            }
        }
        return doPage;
    }

    private void appendSelectedUsers(Page<UserDTO> userPage, List<Long> selectedUserIds, PageRequest pageRequest) {
        boolean append = !ObjectUtils.isEmpty(selectedUserIds) && pageRequest.getPage() == 0;
        if (append) {
            // 拼接选中的用户
            List<UserDTO> list = baseFeignClient.listUsersByIds(selectedUserIds.toArray(new Long[selectedUserIds.size()]), true).getBody();
            if (!CollectionUtils.isEmpty(userPage.getContent())) {
                list.addAll(userPage.getContent());
            }
            userPage.setContent(list);
        }
    }
}

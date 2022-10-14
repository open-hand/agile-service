package io.choerodon.agile.app.service.impl;


import io.choerodon.agile.api.vo.IssueLabelVO;
import io.choerodon.agile.app.service.IssueLabelService;
import io.choerodon.agile.infra.utils.RedisUtil;
import io.choerodon.agile.infra.dto.IssueLabelDTO;
import io.choerodon.agile.infra.mapper.IssueLabelMapper;
import io.choerodon.core.exception.CommonException;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.*;

/**
 * 敏捷开发Issue标签
 *
 * @author dinghuang123@gmail.com
 * @since 2018-05-14 21:04:00
 */
@Service
public class IssueLabelServiceImpl implements IssueLabelService {

    private static final String INSERT_ERROR = "error.IssueLabel.insert";
    private static final String AGILE = "Agile:";
    private static final String LABEL = "label";
    private static final String PIE_CHART = AGILE + "PieChart";

    @Autowired
    private IssueLabelMapper issueLabelMapper;

    @Autowired
    private RedisUtil redisUtil;
    @Autowired
    private ModelMapper modelMapper;

    @Override
    public List<IssueLabelVO> listIssueLabel(Long projectId) {
        List<IssueLabelVO> issueLabelVOS = issueLabelMapper.listByProjectId(projectId);
        if (CollectionUtils.isEmpty(issueLabelVOS)) {
            return new ArrayList<>();
        }
        return issueLabelVOS;
    }

    @Override
    public IssueLabelDTO createBase(IssueLabelDTO issueLabelDTO) {
        String name = issueLabelDTO.getLabelName();
        if (name.length() > 20) {
            throw new CommonException("error.label.name.length");
        }
        if (issueLabelMapper.insert(issueLabelDTO) != 1) {
            throw new CommonException(INSERT_ERROR);
        }
        redisUtil.deleteRedisCache(new String[]{PIE_CHART + issueLabelDTO.getProjectId() + ':' + LABEL + "*"});
        return issueLabelMapper.selectByPrimaryKey(issueLabelDTO.getLabelId());
    }

    @Override
    public int labelGarbageCollection(Long projectId) {
        redisUtil.deleteRedisCache(new String[]{PIE_CHART + projectId + ':' + LABEL + "*"});
        return issueLabelMapper.labelGarbageCollection(projectId);
    }
}
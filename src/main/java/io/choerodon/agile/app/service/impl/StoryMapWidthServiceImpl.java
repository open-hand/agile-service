package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.StoryMapWidthVO;
import io.choerodon.agile.api.validator.StoryMapWidthValidator;
import io.choerodon.agile.app.service.StoryMapWidthService;
import io.choerodon.agile.infra.dto.StoryMapWidthDTO;
import io.choerodon.agile.infra.mapper.StoryMapWidthMapper;
import io.choerodon.core.exception.CommonException;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;


/**
 * Created by HuangFuqiang@choerodon.io on 2019/6/3.
 * Email: fuqianghuang01@gmail.com
 */
@Service
public class StoryMapWidthServiceImpl implements StoryMapWidthService {

    @Autowired
    private StoryMapWidthValidator storyMapWidthValidator;

    @Autowired
    private StoryMapWidthMapper storyMapWidthMapper;
    @Autowired
    private ModelMapper modelMapper;

    @Override
    public StoryMapWidthVO create(Long projectId, StoryMapWidthVO storyMapWidthVO) {
        storyMapWidthValidator.checkStoryMapWidthCreate(storyMapWidthVO);
        StoryMapWidthDTO storyMapWidthDTO = modelMapper.map(storyMapWidthVO, StoryMapWidthDTO.class);
        if (storyMapWidthMapper.insert(storyMapWidthDTO) != 1) {
            throw new CommonException("error.storyMapWidthDTO.insert");
        }
        return modelMapper.map(storyMapWidthMapper.selectByPrimaryKey(storyMapWidthDTO.getId()), StoryMapWidthVO.class);
    }

    @Override
    public StoryMapWidthVO update(Long projectId, StoryMapWidthVO storyMapWidthVO) {
        storyMapWidthValidator.checkStoryMapWidthUpdate(storyMapWidthVO);
        StoryMapWidthDTO storyMapWidthDTO = modelMapper.map(storyMapWidthVO, StoryMapWidthDTO.class);
        if (storyMapWidthMapper.updateByPrimaryKeySelective(storyMapWidthDTO) != 1) {
            throw new CommonException("error.storyMapWidthDTO.update");
        }
        return modelMapper.map(storyMapWidthMapper.selectByPrimaryKey(storyMapWidthDTO.getId()), StoryMapWidthVO.class);
    }

}

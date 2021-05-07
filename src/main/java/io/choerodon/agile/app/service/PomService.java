package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.AppServiceRepVO;
import io.choerodon.agile.api.vo.PublishVersionVO;
import org.xml.sax.SAXException;

import javax.xml.parsers.ParserConfigurationException;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;

/**
 * @author superlee
 * @since 2021-03-12
 */
public interface PomService {

    List<PublishVersionVO> parse(String groupIds,
                                 InputStream inputStream,
                                 List<AppServiceRepVO> appServiceRepList,
                                 PublishVersionVO self)
            throws ParserConfigurationException, IOException, SAXException;
}

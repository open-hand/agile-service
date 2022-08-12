package io.choerodon.agile.infra.utils;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import org.apache.commons.lang.StringEscapeUtils;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;

/**
 * Copyright (c) 2022. Hand Enterprise Solution Company. All right reserved.
 *
 * @author zongqi.hao@zknow.com
 * @since 2022/7/12
 */
public class RichTextUtil {

    private static final String INSERT = "insert";

    public static String getDes(String str) {
        if (str == null) {
            return "";
        }
        StringBuilder result = new StringBuilder();
        try {
            JSONArray root = JSON.parseArray(str);
            for (Object o : root) {
                JSONObject object = (JSONObject) o;
                if (!(object.get(INSERT) instanceof JSONObject)) {
                    result.append(StringEscapeUtils.unescapeJava(object.getString(INSERT)));
                }
            }
        } catch (Exception e) {
            Document doc = Jsoup.parse(str);
            doc.body().children().forEach(element -> {
                String tagName = element.tag().getName();
                if (tagName == null) {
                    result.append(element.text()).append("\n");
                    return;
                }
                switch (tagName) {
                    case "figure":
                        break;
                    case "ol":
                    case "ul":
                        setListElementStr(result, element);
                        break;
                    default:
                        result.append(element.text()).append("\n");
                        break;
                }
            });
        }
        // 纯文本格式直接返回
        return result.length() < 1 ? str : result.toString().trim();
    }

    private static String setListElementStr(StringBuilder result, Element element) {
        element.children().forEach(childElement -> result.append(getLiText(childElement)).append("\n"));
        return element.text();
    }

    private static String getLiText(Element element) {
        StringBuilder result = new StringBuilder();
        String liAllText = element.text();
        StringBuilder childListText = new StringBuilder();
        StringBuilder childRelText = new StringBuilder();
        element.children().forEach(childElement -> {
            String tagName = childElement.tag().getName();
            if ("ol".equals(tagName) || "ul".equals(tagName)) {
                childListText.append(" ").append(setListElementStr(childRelText, childElement));
            }
        });
        if (childListText.length() > 0) {
            int childTextStart = liAllText.indexOf(childListText.toString());
            if (childTextStart > -1) {
                result.append(liAllText, 0, childTextStart);
            } else {
                result.append(liAllText);
            }
            result.append("\n").append(childRelText.toString());
        } else {
            result.append(liAllText);
        }
        return result.toString().trim();
    }

}

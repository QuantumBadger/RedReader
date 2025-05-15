/*******************************************************************************
 * This file is part of RedReader.
 *
 * RedReader is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * RedReader is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with RedReader.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/
package org.quantumbadger.redreader.test.reddit

import org.junit.Assert
import org.junit.Test
import org.quantumbadger.redreader.common.UriString
import org.quantumbadger.redreader.reddit.kthings.JsonUtils
import org.quantumbadger.redreader.reddit.kthings.RedditThing

class ImageTests {

	@Test
	fun i_redd_it_GIF() {
		val json = """
			{
          "kind": "t3",
          "data": {
            "approved_at_utc": null,
            "subreddit": "AnimalsBeingJerks",
            "selftext": "",
            "user_reports": [],
            "saved": false,
            "mod_reason_title": null,
            "gilded": 0,
            "clicked": false,
            "title": "Cat 'boxing'.",
            "link_flair_richtext": [],
            "subreddit_name_prefixed": "r/AnimalsBeingJerks",
            "hidden": false,
            "pwls": 6,
            "link_flair_css_class": "hide img cat",
            "downs": 0,
            "thumbnail_height": 140,
            "top_awarded_type": null,
            "parent_whitelist_status": "all_ads",
            "hide_score": false,
            "name": "t3_188myt8",
            "quarantine": false,
            "link_flair_text_color": "dark",
            "upvote_ratio": 0.97,
            "author_flair_background_color": null,
            "ups": 1768,
            "domain": "i.redd.it",
            "media_embed": {},
            "thumbnail_width": 140,
            "author_flair_template_id": null,
            "is_original_content": false,
            "author_fullname": "t2_bk9bymkz",
            "secure_media": null,
            "is_reddit_media_domain": true,
            "is_meta": false,
            "category": null,
            "secure_media_embed": {},
            "link_flair_text": "cat",
            "can_mod_post": false,
            "score": 1768,
            "approved_by": null,
            "is_created_from_ads_ui": false,
            "author_premium": false,
            "thumbnail": "https://b.thumbs.redditmedia.com/3tU4wwMl_Mb57IAWfvz5U2GFQVv4ZCxxmPQT-SeWyWA.jpg",
            "edited": false,
            "author_flair_css_class": null,
            "author_flair_richtext": [],
            "gildings": {},
            "post_hint": "image",
            "content_categories": null,
            "is_self": false,
            "subreddit_type": "public",
            "created": 1701466148,
            "link_flair_type": "text",
            "wls": 6,
            "removed_by_category": null,
            "banned_by": null,
            "author_flair_type": "text",
            "total_awards_received": 0,
            "allow_live_comments": false,
            "selftext_html": null,
            "likes": null,
            "suggested_sort": null,
            "banned_at_utc": null,
            "url_overridden_by_dest": "https://i.redd.it/kcxj4pb54r3c1.gif",
            "view_count": null,
            "archived": false,
            "no_follow": false,
            "is_crosspostable": true,
            "pinned": false,
            "over_18": false,
            "preview": {
              "images": [
                {
                  "source": {
                    "url": "https://preview.redd.it/kcxj4pb54r3c1.gif?format=png8&amp;s=68dbf944c955011b344c7ee3b63b21b2f702ee43",
                    "width": 600,
                    "height": 750
                  },
                  "resolutions": [
                    {
                      "url": "https://preview.redd.it/kcxj4pb54r3c1.gif?width=108&amp;crop=smart&amp;format=png8&amp;s=8739cfbff956886e2d40803b57b36f370235a383",
                      "width": 108,
                      "height": 135
                    },
                    {
                      "url": "https://preview.redd.it/kcxj4pb54r3c1.gif?width=216&amp;crop=smart&amp;format=png8&amp;s=17ee083f50e7c9bc1d934dfb48670067d0fe13d8",
                      "width": 216,
                      "height": 270
                    },
                    {
                      "url": "https://preview.redd.it/kcxj4pb54r3c1.gif?width=320&amp;crop=smart&amp;format=png8&amp;s=6d30ad7c5e726a7ac3038213269d1d913634bea4",
                      "width": 320,
                      "height": 400
                    }
                  ],
                  "variants": {
                    "gif": {
                      "source": {
                        "url": "https://preview.redd.it/kcxj4pb54r3c1.gif?s=d7291196d3e8816aa43df8c76ef868ecc04565ff",
                        "width": 600,
                        "height": 750
                      },
                      "resolutions": [
                        {
                          "url": "https://preview.redd.it/kcxj4pb54r3c1.gif?width=108&amp;crop=smart&amp;s=a83a1943b0d37a8004ad4a987ff3e42a363814a1",
                          "width": 108,
                          "height": 135
                        },
                        {
                          "url": "https://preview.redd.it/kcxj4pb54r3c1.gif?width=216&amp;crop=smart&amp;s=13538abf9ec944d1570854768868a585b9654294",
                          "width": 216,
                          "height": 270
                        },
                        {
                          "url": "https://preview.redd.it/kcxj4pb54r3c1.gif?width=320&amp;crop=smart&amp;s=b6b3df898f6b3d289a1e63b44e2552a2a2755907",
                          "width": 320,
                          "height": 400
                        }
                      ]
                    },
                    "mp4": {
                      "source": {
                        "url": "https://preview.redd.it/kcxj4pb54r3c1.gif?format=mp4&amp;s=10d866af8122f89103f57b6ee19a64cd3fefaf14",
                        "width": 600,
                        "height": 750
                      },
                      "resolutions": [
                        {
                          "url": "https://preview.redd.it/kcxj4pb54r3c1.gif?width=108&amp;format=mp4&amp;s=ddcfca15d0ad2821b2718c0c5932d91e5c84d224",
                          "width": 108,
                          "height": 135
                        },
                        {
                          "url": "https://preview.redd.it/kcxj4pb54r3c1.gif?width=216&amp;format=mp4&amp;s=a15381e681cd70abab2d6689000291844030f53f",
                          "width": 216,
                          "height": 270
                        },
                        {
                          "url": "https://preview.redd.it/kcxj4pb54r3c1.gif?width=320&amp;format=mp4&amp;s=09ecdd823f921b7a3ee98d1c3d8b06739bc2a60e",
                          "width": 320,
                          "height": 400
                        }
                      ]
                    }
                  },
                  "id": "nEKXT3PCGaRewPKA9wcbEhW_bCSMiVKjIU-eK0Ig2Co"
                }
              ],
              "enabled": true
            },
            "all_awardings": [],
            "awarders": [],
            "media_only": false,
            "link_flair_template_id": "d14e564e-2b1c-11e3-9d92-12313b04c5c2",
            "can_gild": false,
            "spoiler": false,
            "locked": false,
            "author_flair_text": null,
            "treatment_tags": [],
            "visited": false,
            "removed_by": null,
            "mod_note": null,
            "distinguished": null,
            "subreddit_id": "t5_2wfjv",
            "author_is_blocked": false,
            "mod_reason_by": null,
            "num_reports": null,
            "removal_reason": null,
            "link_flair_background_color": "",
            "id": "188myt8",
            "is_robot_indexable": true,
            "num_duplicates": 0,
            "report_reasons": null,
            "author": "morganmonroe81",
            "discussion_type": null,
            "num_comments": 31,
            "send_replies": false,
            "media": null,
            "contest_mode": false,
            "author_patreon_flair": false,
            "author_flair_text_color": null,
            "permalink": "/r/AnimalsBeingJerks/comments/188myt8/cat_boxing/",
            "whitelist_status": "all_ads",
            "stickied": false,
            "url": "https://i.redd.it/kcxj4pb54r3c1.gif",
            "subreddit_subscribers": 6541852,
            "created_utc": 1701466148,
            "num_crossposts": 1,
            "mod_reports": [],
            "is_video": false
          }
        }
		"""

		val post = JsonUtils.decodeRedditThingFromStream(json.byteInputStream())

		Assert.assertEquals(
			UriString("https://preview.redd.it/kcxj4pb54r3c1.gif?format=mp4&s=10d866af8122f89103f57b6ee19a64cd3fefaf14"),
			(post as RedditThing.Post).data.findUrl())
	}

	@Test
	fun redditGalleryWithUrlOverride() {
		val json = """
			{
          "kind": "t3",
          "data": {
            "approved_at_utc": null,
            "subreddit": "test",
            "selftext": "",
            "user_reports": [],
            "saved": false,
            "mod_reason_title": null,
            "gilded": 0,
            "clicked": false,
            "is_gallery": true,
            "title": "test",
            "link_flair_richtext": [],
            "subreddit_name_prefixed": "r/test",
            "hidden": false,
            "pwls": null,
            "link_flair_css_class": null,
            "downs": 0,
            "thumbnail_height": 70,
            "top_awarded_type": null,
            "parent_whitelist_status": null,
            "name": "t3_13v4t0m",
            "media_metadata": {
              "fjkymf2aav2b1": {
                "status": "valid",
                "e": "Image",
                "m": "image/png",
                "p": [],
                "s": {
                  "y": 100,
                  "x": 100,
                  "u": "https://preview.redd.it/fjkymf2aav2b1.png?width=100&amp;format=png&amp;auto=webp&amp;s=e9f5231ddf13b52c482c82a9e32bc52bbdfe4a0a"
                },
                "id": "fjkymf2aav2b1"
              },
              "r8vamtt9av2b1": {
                "status": "valid",
                "e": "Image",
                "m": "image/png",
                "p": [],
                "s": {
                  "y": 100,
                  "x": 100,
                  "u": "https://preview.redd.it/r8vamtt9av2b1.png?width=100&amp;format=png&amp;auto=webp&amp;s=adf9b825ca4debb649faf5009d684fc7ac4335a8"
                },
                "id": "r8vamtt9av2b1"
              }
            },
            "hide_score": false,
            "quarantine": false,
            "link_flair_text_color": "dark",
            "upvote_ratio": 1,
            "author_flair_background_color": null,
            "ups": 1,
            "domain": "old.reddit.com",
            "media_embed": {},
            "thumbnail_width": 70,
            "author_flair_template_id": null,
            "is_original_content": false,
            "author_fullname": "t2_cbh9wklyk",
            "secure_media": null,
            "is_reddit_media_domain": false,
            "is_meta": false,
            "category": null,
            "secure_media_embed": {},
            "gallery_data": {
              "items": [
                {
                  "caption": "test #1",
                  "outbound_url": "https://example.com/",
                  "media_id": "r8vamtt9av2b1",
                  "id": 281271232
                },
                {
                  "caption": "test #2",
                  "outbound_url": "https://example.net/",
                  "media_id": "fjkymf2aav2b1",
                  "id": 281271233
                }
              ]
            },
            "link_flair_text": null,
            "can_mod_post": false,
            "score": 1,
            "approved_by": null,
            "is_created_from_ads_ui": false,
            "author_premium": false,
            "thumbnail": "https://b.thumbs.redditmedia.com/y9gPjunJVfn61TdJ-YjC2o7gtuH0AKgYkk_c8tz0kzg.jpg",
            "edited": false,
            "author_flair_css_class": null,
            "author_flair_richtext": [],
            "gildings": {},
            "content_categories": null,
            "is_self": false,
            "subreddit_type": "public",
            "created": 1685390255,
            "link_flair_type": "text",
            "wls": null,
            "removed_by_category": null,
            "banned_by": null,
            "author_flair_type": "text",
            "total_awards_received": 0,
            "allow_live_comments": false,
            "selftext_html": null,
            "likes": null,
            "suggested_sort": null,
            "banned_at_utc": null,
            "url_overridden_by_dest": "https://www.reddit.com/gallery/13v4t0m",
            "view_count": null,
            "archived": false,
            "no_follow": true,
            "is_crosspostable": true,
            "pinned": false,
            "over_18": false,
            "all_awardings": [],
            "awarders": [],
            "media_only": false,
            "can_gild": false,
            "spoiler": false,
            "locked": false,
            "author_flair_text": null,
            "treatment_tags": [],
            "visited": false,
            "removed_by": null,
            "mod_note": null,
            "distinguished": null,
            "subreddit_id": "t5_2qh23",
            "author_is_blocked": false,
            "mod_reason_by": null,
            "num_reports": null,
            "removal_reason": null,
            "link_flair_background_color": "",
            "id": "13v4t0m",
            "is_robot_indexable": true,
            "num_duplicates": 29,
            "report_reasons": null,
            "author": "Emotional-Track-6160",
            "discussion_type": null,
            "num_comments": 0,
            "send_replies": true,
            "media": null,
            "contest_mode": false,
            "author_patreon_flair": false,
            "author_flair_text_color": null,
            "permalink": "/r/test/comments/13v4t0m/test/",
            "whitelist_status": null,
            "stickied": false,
            "url": "https://example.com/",
            "subreddit_subscribers": 18095,
            "created_utc": 1685390255,
            "num_crossposts": 0,
            "mod_reports": [],
            "is_video": false
          }
        }
		"""

		val post = JsonUtils.decodeRedditThingFromStream(json.byteInputStream())

		Assert.assertEquals(UriString("https://www.reddit.com/gallery/13v4t0m"), (post as RedditThing.Post).data.findUrl())
	}
}
